`include "lib/defines.vh"
module EX(
    input wire clk,
    input wire rst,
    // input wire flush,
    input wire [`StallBus-1:0] stall,

    input wire [`ID_TO_EX_WD-1+64:0] id_to_ex_bus,

    output wire [`EX_TO_MEM_WD-1+64+1+2+32:0] ex_to_mem_bus,

    output wire data_sram_en,
    output wire [3:0] data_sram_wen,
    output wire [31:0] data_sram_addr,
    output wire [31:0] data_sram_wdata,
    //�Լ��ӵ�
    output wire [37+64+1+2:0] ex_to_id_bus,
    output wire ex_is_load,
    output wire stallreq_for_ex
);

    reg [`ID_TO_EX_WD-1+64:0] id_to_ex_bus_r;

    always @ (posedge clk) begin
        if (rst) begin
            id_to_ex_bus_r <= `ID_TO_EX_WD+64'b0;
        end
        // else if (flush) begin
        //     id_to_ex_bus_r <= `ID_TO_EX_WD'b0;
        // end
        else if (stall[2]==`Stop && stall[3]==`NoStop) begin
            id_to_ex_bus_r <= `ID_TO_EX_WD+64'b0;
        end
        else if (stall[2]==`NoStop) begin
            id_to_ex_bus_r <= id_to_ex_bus;
        end
    end

    wire [31:0] ex_pc, inst;
    wire [11:0] alu_op;
    wire [2:0] sel_alu_src1;
    wire [3:0] sel_alu_src2;
    wire data_ram_en;
    wire [3:0] data_ram_wen;
    wire rf_we;
    wire [4:0] rf_waddr;
    wire sel_rf_res;
    wire [31:0] rf_rdata1, rf_rdata2;
    reg is_in_delayslot;
    

    wire [31:0] HI;
    wire [31:0] LO;
    assign {
        HI,             
        LO,             
        ex_pc,          // 148:117
        inst,           // 116:85
        alu_op,         // 84:83
        sel_alu_src1,   // 82:80
        sel_alu_src2,   // 79:76
        data_ram_en,    // 75
        data_ram_wen,   // 74:71
        rf_we,          // 70
        rf_waddr,       // 69:65
        sel_rf_res,     // 64
        rf_rdata1,         // 63:32
        rf_rdata2          // 31:0
    } = id_to_ex_bus_r;
    
    assign ex_is_load = (inst[31:26] == 6'b10_0011) ? 1'b1 : 1'b0;
    
    wire [31:0] imm_sign_extend, imm_zero_extend, sa_zero_extend;
    assign imm_sign_extend = {{16{inst[15]}},inst[15:0]};
    assign imm_zero_extend = {16'b0, inst[15:0]};
    assign sa_zero_extend = {27'b0,inst[10:6]};

    wire [31:0] alu_src1, alu_src2;
    wire [31:0] alu_result, ex_result;

    assign alu_src1 = sel_alu_src1[1] ? ex_pc :
                      sel_alu_src1[2] ? sa_zero_extend : rf_rdata1;

    assign alu_src2 = sel_alu_src2[1] ? imm_sign_extend :
                      sel_alu_src2[2] ? 32'd8 :
                      sel_alu_src2[3] ? imm_zero_extend : rf_rdata2;
    
    alu u_alu(
    	.alu_control (alu_op ),
        .alu_src1    (alu_src1    ),
        .alu_src2    (alu_src2    ),
        .alu_result  (alu_result  )
    );

    assign ex_result = (inst[31:26]==6'b000000 & inst[5:0]==6'b010010) ? LO 
    : (inst[31:26]==6'b000000 & inst[5:0]==6'b010000) ? HI : alu_result;

    wire [31:0] rf_rdata22; 
    assign rf_rdata22 = inst[31:26]==6'b101000 ? {4{rf_rdata2[7:0]}}
    : inst[31:26]==6'b101001 ? {2{rf_rdata2[15:0]}}
    : rf_rdata2;
    wire [3:0] data_ram_wen2;
    assign data_ram_wen2 = (inst[31:26]==6'b101000) ? ( (ex_result[1]==1'b0 & ex_result[0]==1'b0) ? 4'b0001
    : (ex_result[0]==1'b1 & ex_result[1]==1'b0) ? 4'b0010
    : (ex_result[0]==1'b0 & ex_result[1]==1'b1) ? 4'b0100
    : (ex_result[0]==1'b1 & ex_result[1]==1'b1) ? 4'b1000
    : data_ram_wen )
    : (inst[31:26]==6'b101001) ? ( ex_result[1]==1'b0 ? 4'b0011
    : ex_result[1]==1'b1 ? 4'b1100
    : data_ram_wen )
    : data_ram_wen;
    
    assign data_sram_en = data_ram_en;
    assign data_sram_wen = data_ram_wen2;
    assign data_sram_addr = ex_result;
    assign data_sram_wdata = rf_rdata22;
    
    // MUL part
    wire mul_flag;
    wire [63:0] mul_result;
    wire mul_signed; // �з��ų˷����
    wire [31:0]alu_src11;
    wire [31:0]alu_src22;
    assign inst_mult = (inst[31:26]==6'b000000 & inst[5:0]==6'b011000) ? 1'b1 : 1'b0;
    assign inst_multu = (inst[31:26]==6'b000000 & inst[5:0]==6'b011001) ? 1'b1 : 1'b0;
    assign mul_flag = inst_mult | inst_multu ;
    assign mul_signed = inst_mult;
    assign alu_src11 = mul_flag ? alu_src1 : 32'b0;
    
    assign alu_src22 = mul_flag ? alu_src2 : 32'b0;
    mul u_mul(
    	.clk        (clk            ),
        .resetn     (~rst           ),
        .mul_signed (mul_signed     ),
        .ina        (alu_src11      ), // �˷�Դ������1
        .inb        (alu_src22      ), // �˷�Դ������2
        .result     (mul_result     ) // �˷���� 64bit
    );

    // DIV part
    wire div_flag;
    wire [63:0] div_result;
    wire inst_div, inst_divu;
    wire div_ready_i;
    reg stallreq_for_div;
    assign stallreq_for_ex = stallreq_for_div;

    reg [31:0] div_opdata1_o;
    reg [31:0] div_opdata2_o;
    reg div_start_o;
    reg signed_div_o;
    
    assign inst_div  = (inst[31:26] == 6'b000000) & (inst[5:0] == 6'b011010) ? 1'b1 : 1'b0 ;
    assign inst_divu = (inst[31:26] == 6'b000000) & (inst[5:0] == 6'b011011) ? 1'b1 : 1'b0 ;
    assign div_flag = inst_div | inst_divu;
    
    div u_div(
    	.rst          (rst          ),
        .clk          (clk          ),
        .signed_div_i (signed_div_o ),
        .opdata1_i    (div_opdata1_o    ),
        .opdata2_i    (div_opdata2_o    ),
        .start_i      (div_start_o      ),
        .annul_i      (1'b0      ),
        .result_o     (div_result     ), // ������� 64bit
        .ready_o      (div_ready_i      )
    );

    always @ (*) begin
        if (rst) begin
            stallreq_for_div = `NoStop;
            div_opdata1_o = `ZeroWord;
            div_opdata2_o = `ZeroWord;
            div_start_o = `DivStop;
            signed_div_o = 1'b0;
        end
        else begin
            stallreq_for_div = `NoStop;
            div_opdata1_o = `ZeroWord;
            div_opdata2_o = `ZeroWord;
            div_start_o = `DivStop;
            signed_div_o = 1'b0;
            case ({inst_div,inst_divu})
                2'b10:begin
                    if (div_ready_i == `DivResultNotReady) begin
                        div_opdata1_o = rf_rdata1;
                        div_opdata2_o = rf_rdata2;
                        div_start_o = `DivStart;
                        signed_div_o = 1'b1;
                        stallreq_for_div = `Stop;
                    end
                    else if (div_ready_i == `DivResultReady) begin
                        div_opdata1_o = rf_rdata1;
                        div_opdata2_o = rf_rdata2;
                        div_start_o = `DivStop;
                        signed_div_o = 1'b1;
                        stallreq_for_div = `NoStop;
                    end
                    else begin
                        div_opdata1_o = `ZeroWord;
                        div_opdata2_o = `ZeroWord;
                        div_start_o = `DivStop;
                        signed_div_o = 1'b0;
                        stallreq_for_div = `NoStop;
                    end
                end
                2'b01:begin
                    if (div_ready_i == `DivResultNotReady) begin
                        div_opdata1_o = rf_rdata1;
                        div_opdata2_o = rf_rdata2;
                        div_start_o = `DivStart;
                        signed_div_o = 1'b0;
                        stallreq_for_div = `Stop;
                    end
                    else if (div_ready_i == `DivResultReady) begin
                        div_opdata1_o = rf_rdata1;
                        div_opdata2_o = rf_rdata2;
                        div_start_o = `DivStop;
                        signed_div_o = 1'b0;
                        stallreq_for_div = `NoStop;
                    end
                    else begin
                        div_opdata1_o = `ZeroWord;
                        div_opdata2_o = `ZeroWord;
                        div_start_o = `DivStop;
                        signed_div_o = 1'b0;
                        stallreq_for_div = `NoStop;
                    end
                end
                default:begin
                end
            endcase
        end
    end
    
    wire flag;
    wire[1:0] mt_flag;
    wire [63:0] result;
    wire [31:0] ex_result2;
    assign mt_flag[0] = (inst[31:26] == 6'b000000 & inst[5:0] == 6'b010011) ? 1'b1 : 1'b0; //lo
    assign mt_flag[1] = (inst[31:26] == 6'b000000 & inst[5:0] == 6'b010001) ? 1'b1 : 1'b0;  //hi
    assign flag = mul_flag | div_flag ;
    assign result = mul_flag ? mul_result : div_flag ? div_result : 64'b0 ;
    assign ex_result2 = ( mt_flag[0] | mt_flag[1] ) ? rf_rdata1 : ex_result;
    // mul_result �� div_result ����ֱ��ʹ��
    assign ex_to_mem_bus = {
        mt_flag,     //2
        flag,         
        result,
        inst,      
        ex_pc,          // 75:44
        data_ram_en,    // 43
        data_ram_wen,   // 42:39
        sel_rf_res,     // 38
        rf_we,          // 37
        rf_waddr,       // 36:32
        ex_result2       // 31:0
    };
    assign ex_to_id_bus={
//        hi_wen,         
//        lo_wen,         
//        div_result,
        mt_flag,
        flag,         
        result,      
        rf_we,
        rf_waddr,
        ex_result2
    };
endmodule