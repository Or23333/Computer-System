`include "lib/defines.vh"
module ID(

    input wire clk,
    input wire rst,
    // input wire flush,
    input wire [`StallBus-1:0] stall,
    output wire stallreq_from_id,
    //
    input wire [`IF_TO_ID_WD-1:0] if_to_id_bus,

    input wire [31:0] inst_sram_rdata,

    input wire [`WB_TO_RF_WD-1+64+1+2:0] wb_to_rf_bus,

    output wire [`ID_TO_EX_WD-1+64:0] id_to_ex_bus,

    output wire [`BR_WD-1:0] br_bus,
    ///定向  自己添加的
    input wire [37+64+1+2:0] ex_to_id_bus,
    input wire [37+64+1+2:0] mem_to_id_bus,
    input wire  ex_is_load 
);

    reg [`IF_TO_ID_WD-1:0] if_to_id_bus_r;
    wire [31:0] inst;
    wire [31:0] id_pc;
    wire ce;
   
    wire wb_id_div_flag;
    wire [63:0]  wb_id_div_result;
    wire wb_rf_we;
    wire [4:0] wb_rf_waddr;
    wire [31:0] wb_rf_wdata;
    wire [1:0] wb_id_mt_flag;
    
    always @ (posedge clk) begin
        if (rst) begin
            if_to_id_bus_r <= `IF_TO_ID_WD'b0;        
        end
        else if (stall[1]==`Stop && stall[2]==`NoStop) begin
            if_to_id_bus_r <= `IF_TO_ID_WD'b0;
        end
        else if (stall[1]==`NoStop) begin
            if_to_id_bus_r <= if_to_id_bus;
        end
    end
    
    assign {
        ce,
        id_pc
    } = if_to_id_bus_r;
    assign {
        wb_id_mt_flag,
        wb_id_div_flag,         
        wb_id_div_result,      
        wb_rf_we,
        wb_rf_waddr,
        wb_rf_wdata
    } = wb_to_rf_bus;

    wire [5:0] opcode;
    wire [4:0] rs,rt,rd,sa;
    wire [5:0] func;
    wire [15:0] imm;
    wire [25:0] instr_index;
    wire [19:0] code;
    wire [4:0] base;
    wire [15:0] offset;
    wire [2:0] sel;

    wire [63:0] op_d, func_d;
    wire [31:0] rs_d, rt_d, rd_d, sa_d;

    wire [2:0] sel_alu_src1;
    wire [3:0] sel_alu_src2;
    wire [11:0] alu_op;

    wire data_ram_en;
    wire [3:0] data_ram_wen;
    
    wire rf_we;
    wire [4:0] rf_waddr;
    wire sel_rf_res;
    wire [2:0] sel_rf_dst;

    wire [31:0] rdata1, rdata2;
    
    reg[31:0] HI;
    reg[31:0] LO;
    wire hi_en,hi_wen,lo_en,lo_wen;
    
    regfile u_regfile(
    	.clk    (clk    ),
        .raddr1 (rs ),
        .rdata1 (rdata1 ),
        .raddr2 (rt ),
        .rdata2 (rdata2 ),
        .we     (wb_rf_we     ),
        .waddr  (wb_rf_waddr  ),
        .wdata  (wb_rf_wdata  )
    );

    always @ (posedge clk) begin
        if(wb_id_div_flag == 1'b1) begin
            LO <= wb_id_div_result[31:0];
            HI <= wb_id_div_result[63:32];
        end
        else if (wb_id_mt_flag[0] == 1'b1) begin
            LO <=  wb_rf_wdata;
        end
        else if (wb_id_mt_flag[1] == 1'b1) begin
            HI <=  wb_rf_wdata;
        end
        
    end
    
    assign opcode = inst[31:26];
    assign rs = inst[25:21];
    assign rt = inst[20:16];
    assign rd = inst[15:11];
    assign sa = inst[10:6];
    assign func = inst[5:0];
    assign imm = inst[15:0];
    assign instr_index = inst[25:0];
    assign code = inst[25:6];
    assign base = inst[25:21];
    assign offset = inst[15:0];
    assign sel = inst[2:0];
    //指令集
    wire inst_ori, inst_lui, inst_addiu, inst_beq, 
    inst_subu, //rs-rt->rd
    //inst_jr;    //pc ← rs，将地址为rs的通用寄存器的值赋给寄存器PC，作为新的指令地址。
    inst_jal,   //无条件跳转。跳转目标由该分支指令对应的延迟槽指令的 PC 的最高 4 位与立即数 instr_index 左移2 位后的值拼接得到。同时将该分支对应延迟槽指令之后的指令的 PC 值保存至第 31 号通用寄存器中。
                //I: GPR[31] ← PC + 8
    inst_jr,    //无条件跳转。跳转目标为寄存器 rs 中的值。
    inst_addu,  //将寄存器 rs 的值与寄存器 rt 的值相加，结果写入 rd 寄存器中。
    inst_sll,   //由立即数 sa 指定移位量，对寄存器 rt 的值进行逻辑左移，结果写入寄存器 rd 中。
    inst_or,    //寄存器 rs 中的值与寄存器 rt 中的值按位逻辑或，结果写入寄存器 rd 中
    inst_lw,    //将 base 寄存器的值加上符号扩展后的立即数 offset 得到访存的虚地址，如果地址不是 4 的整数倍
                //则触发地址错例外，否则据此虚地址从存储器中读取连续 4 个字节的值，写入到 rt 寄存器中。
    inst_bne, inst_sltu, inst_slt, inst_slti, inst_sltiu , inst_j,
    inst_add , inst_addi, inst_sub, inst_and, inst_andi, inst_nor,
    inst_xori, inst_sllv, inst_sra, inst_srav, inst_srl, inst_srlv,
    inst_bgez, inst_bgtz, inst_blez, inst_bltzal, inst_bgezal ,
    inst_jalr,
//    inst_div;
    inst_mflo,
    inst_mfhi,
    inst_lb,
    inst_lbu,
    inst_lh,
    inst_lhu,
    inst_sb,
    inst_sh;
    
               
    wire op_add, op_sub, op_slt, op_sltu;
    wire op_and, op_nor, op_or, op_xor;
    wire op_sll, op_srl, op_sra, op_lui;

    decoder_6_64 u0_decoder_6_64(
    	.in  (opcode  ),
        .out (op_d )
    );

    decoder_6_64 u1_decoder_6_64(
    	.in  (func  ),
        .out (func_d )
    );
    
    decoder_5_32 u0_decoder_5_32(
    	.in  (rs  ),
        .out (rs_d )
    );

    decoder_5_32 u1_decoder_5_32(
    	.in  (rt  ),
        .out (rt_d )
    );

    //指令集独热编码
    assign inst_ori     = op_d[6'b00_1101];
    assign inst_lui     = op_d[6'b00_1111];
    assign inst_addiu   = op_d[6'b00_1001];
    assign inst_beq     = op_d[6'b00_0100];
    assign inst_subu    = (op_d[6'b00_0000]&func_d[6'b10_0011]);
    //assign inst_jr      = (op_d[6'b00_0000]&func_d[6'b00_1000]);
    assign inst_jal     = op_d[6'b00_0011];
    assign inst_jr      = (op_d[6'b00_0000]&func_d[6'b00_1000]);
    assign inst_addu    = (op_d[6'b00_0000]&func_d[6'b10_0001]);
    assign inst_sll    = (op_d[6'b00_0000]&func_d[6'b00_0000]);
    assign inst_or    = (op_d[6'b00_0000]&func_d[6'b10_0101]);
    assign inst_lw    =op_d[6'b10_0011];
    assign inst_bne    = op_d[6'b00_0101];
    assign inst_xor    =op_d[6'b00_0000]&func_d[6'b100110]; 
    assign inst_sltu    =op_d[6'b00_0000]&func_d[6'b101011]; 
    assign inst_sw      =  op_d[6'b10_1011];
    assign inst_slt    =op_d[6'b00_0000]&func_d[6'b101010
];
    assign inst_slti    =op_d[6'b00_1010];
    assign inst_sltiu   =op_d[6'b00_1011];
    assign inst_j       =op_d[6'b00_0010];
    assign inst_add       =op_d[6'b00_0000]&func_d[6'b10_0000
];
    assign inst_addi       =op_d[6'b00_1000];
    assign inst_sub        =op_d[6'b00_0000]&func_d[6'b100010]; 
    assign inst_and            =op_d[6'b000000]&func_d[6'b100100];
    assign inst_andi        =op_d[6'b001100];
    assign inst_nor         =op_d[6'b000000]&func_d[6'b100111];
    assign inst_xori        =op_d[6'b001110];
    assign inst_sllv        =op_d[6'b000000]&func_d[6'b000100];
    assign inst_sra         =op_d[6'b000000]&func_d[6'b000011];
    assign inst_srav        =op_d[6'b000000]&func_d[6'b000111];
    assign inst_srl         =op_d[6'b000000]&func_d[6'b000010];
    assign inst_srlv        =op_d[6'b000000]&func_d[6'b000110];
    assign inst_bgez    =  (op_d[6'b00_0001]&rt_d[5'b00_001]);
    assign inst_bgtz    =  (op_d[6'b00_0111]&rt_d[5'b00_000]);
    assign inst_blez    =  (op_d[6'b00_0110]&rt_d[5'b00_000]);
    assign inst_bltz    =  (op_d[6'b00_0001]&rt_d[5'b00_000]);
    assign inst_bltzal  =  (op_d[6'b00_0001]&rt_d[5'b10_000]);
    assign inst_bgezal  =  (op_d[6'b00_0001]&&rt_d[5'b10_001]);
    assign inst_jalr    =  (op_d[6'b00_0000]&rt_d[5'b00_000]&func_d[6'b00_1001]);
    assign inst_mflo    =  (op_d[6'b000000]&func_d[6'b010010]);
    assign inst_mfhi    =  (op_d[6'b000000]&func_d[6'b010000]);
    assign inst_lb      =  op_d[6'b100000];
    assign inst_lbu     =   op_d[6'b100100];
    assign inst_lh      =   op_d[6'b100001];
    assign inst_lhu     =   op_d[6'b100101];
    assign inst_sb      =   op_d[6'b101000];
    assign inst_sh      =   op_d[6'b101001];
    // rs to reg1
    assign sel_alu_src1[0] = inst_ori | inst_addiu | inst_subu 
    | inst_jr | inst_addu | inst_or | inst_lw | inst_xor | inst_sltu | inst_slt 
    | inst_slti | inst_sltiu | inst_add | inst_addi | inst_sub | inst_and | inst_andi
    | inst_nor | inst_xori | inst_sllv | inst_srav | inst_srlv | inst_lb | inst_lbu
    | inst_lh | inst_lhu | inst_sb | inst_sh;

    // pc to reg1
    assign sel_alu_src1[1] = inst_jal | inst_bltzal | inst_bgezal | inst_jalr ;

    // sa_zero_extend to reg1
    assign sel_alu_src1[2] = inst_sll | inst_sra | inst_srl;

    
    // rt to reg2
    assign sel_alu_src2[0] = inst_subu |inst_addu | inst_sll | inst_or 
    | inst_xor | inst_sltu | inst_bne | inst_slt | inst_add | inst_sub
    | inst_and | inst_nor | inst_sllv | inst_sra | inst_srav | inst_srl
    | inst_srlv ;
    
    // imm_sign_extend to reg2
    assign sel_alu_src2[1] = inst_lui | inst_addiu | inst_lw | inst_sw | inst_slti
    | inst_sltiu | inst_addi | inst_lb | inst_lbu | inst_lh | inst_lhu | inst_sb
    | inst_sh;

    // 32'b8 to reg2
    assign sel_alu_src2[2] = inst_jal | inst_bltzal | inst_bgezal |inst_jalr;

    // imm_zero_extend to reg2
    assign sel_alu_src2[3] = inst_ori | inst_andi | inst_xori  ;



    assign op_add = inst_addiu | inst_jal | inst_addu | inst_lw | inst_sw |inst_add |inst_addi
    | inst_bltzal | inst_bgezal | inst_jalr | inst_lb | inst_lbu | inst_lh | inst_lhu | inst_sb
    | inst_sh;
    assign op_sub = inst_subu | inst_sub ;
    assign op_slt = inst_slt | inst_slti;
    assign op_sltu = inst_sltu | inst_sltiu;
    assign op_and = inst_and |inst_andi ;
    assign op_nor = inst_nor;
    assign op_or = inst_ori | inst_or;
    assign op_xor = inst_xor | inst_xori ;
    assign op_sll = inst_sll | inst_sllv;
    assign op_srl = inst_srl |inst_srlv;
    assign op_sra = inst_sra | inst_srav;
    assign op_lui = inst_lui; 

    assign alu_op = {op_add, op_sub, op_slt, op_sltu,
                     op_and, op_nor, op_or, op_xor,
                     op_sll, op_srl, op_sra, op_lui
                     };



    // load and store enable
    assign data_ram_en = inst_lw | inst_sw | inst_lb | inst_lbu | inst_lh | inst_lhu | inst_sb | inst_sw | inst_sh;

    // write enable 0 store  1 load
    assign data_ram_wen = inst_sw ? 4'b1111 : 
    inst_lw ? 4'b0000 : 4'b0000;



    // regfile sotre enable
    assign rf_we = inst_ori | inst_lui | inst_addiu |inst_subu|inst_jal 
    | inst_addu |inst_sll | inst_or | inst_lw | inst_xor | inst_sltu 
    | inst_slt | inst_slti | inst_sltiu | inst_add | inst_addi | inst_sub
    | inst_and |inst_andi | inst_nor | inst_xori | inst_sllv | inst_sra
    | inst_srav | inst_srl | inst_srlv | inst_bltzal | inst_bgezal | inst_jalr
    | inst_mflo | inst_mfhi | inst_lb | inst_lbu | inst_lh | inst_lhu ;



    // store in [rd]
    assign sel_rf_dst[0] = inst_subu | inst_addu | inst_sll | inst_or | inst_xor 
    | inst_sltu | inst_slt |inst_add |inst_sub | inst_and | inst_nor | inst_sllv
    | inst_sra | inst_srav | inst_srl | inst_srlv | inst_jalr | inst_mflo | inst_mfhi;
    // store in [rt] 
    assign sel_rf_dst[1] = inst_ori | inst_lui | inst_addiu | inst_lw | inst_slti 
    | inst_sltiu |inst_addi | inst_andi | inst_xori | inst_lb | inst_lbu | inst_lh | inst_lhu;
    // store in [31]
    assign sel_rf_dst[2] = inst_jal | inst_bltzal | inst_bgezal;

    // sel for regfile address
    assign rf_waddr = {5{sel_rf_dst[0]}} & rd 
                    | {5{sel_rf_dst[1]}} & rt
                    | {5{sel_rf_dst[2]}} & 32'd31;

    // 0 from alu_res ; 1 from ld_res  alu类指令置0，load类指令置1
    assign sel_rf_res = 1'b0; 
    
    //
    wire [31:0] rdata11;
    wire [31:0] rdata22;
    wire [31:0] HI2;
    wire [31:0] LO2;
    //
    assign id_to_ex_bus = {
        HI2,             
        LO2,             
        id_pc,          // 148:117
        inst,           // 116:85
        alu_op,         // 84:83
        sel_alu_src1,   // 82:80
        sel_alu_src2,   // 79:76
        data_ram_en,    // 75
        data_ram_wen,   // 74:71
        rf_we,          // 70
        rf_waddr,       // 69:65
        sel_rf_res,     // 64
        rdata11,         // 63:32
        rdata22          // 31:0
    };

    //ex,mem,wb_to_id
    wire ex_id_we;
    wire [4:0] ex_id_waddr;
    wire [31:0] ex_id_wdata;
    wire [1:0] ex_id_mt_flag;
    wire mem_id_we;
    wire [4:0] mem_id_waddr;
    wire [31:0] mem_id_wdata;
    wire [1:0] mem_id_mt_flag;
    wire wb_id_we;
    wire [4:0] wb_id_waddr;
    wire [31:0] wb_id_wdata;
    wire ex_id_div_flag;
    wire [63:0]ex_id_div_result;
    wire mem_id_div_flag;
    wire [63:0] mem_id_div_result;
    assign {
        ex_id_mt_flag,
        ex_id_div_flag,         
        ex_id_div_result,      
        ex_id_we,
        ex_id_waddr,
        ex_id_wdata
    } = ex_to_id_bus;
    assign {
        mem_id_mt_flag,
        mem_id_div_flag,         
        mem_id_div_result,      
        mem_id_we,
        mem_id_waddr,
        mem_id_wdata
    } = mem_to_id_bus;
     assign wb_id_we=wb_rf_we;
    assign wb_id_waddr=wb_rf_waddr;
    assign wb_id_wdata=wb_rf_wdata; 
    assign rdata11 = (ex_id_we & (ex_id_waddr==rs))? ex_id_wdata: ((mem_id_we &(mem_id_waddr==rs)) ? mem_id_wdata:((wb_id_we &(wb_id_waddr==rs)) ? wb_id_wdata : rdata1));
    assign rdata22 = (ex_id_we & (ex_id_waddr==rt))? ex_id_wdata: ((mem_id_we &(mem_id_waddr==rt)) ? mem_id_wdata: ((wb_id_we &(wb_id_waddr==rt)) ? wb_id_wdata : rdata2));
    assign HI2= ex_id_div_flag ? ex_id_div_result[63:32] : mem_id_div_flag ? mem_id_div_result[63:32] : wb_id_div_flag ? wb_id_div_result[63:32] 
    : ex_id_mt_flag[1] ? ex_id_wdata : mem_id_mt_flag[1] ? mem_id_wdata : HI;
    assign LO2= ex_id_div_flag ? ex_id_div_result[31:0] : mem_id_div_flag ? mem_id_div_result[31:0] : wb_id_div_flag ? wb_id_div_result[31:0] 
    : ex_id_mt_flag[0] ? ex_id_wdata : mem_id_mt_flag[0] ? mem_id_wdata : LO;
    //
    
    
    //新定义一个变量，表示上一条指令是否为加载指令. 如果是 则为 1，否为0
    assign stallreq_from_id = (ex_is_load & ex_id_waddr==rs ) | (ex_is_load & ex_id_waddr==rt ) ;
    
    reg flag,flag2;
    reg [31:0]inst_reg;
    reg [31:0] inst_reg2;
    always @ (posedge clk) begin
        if (stall[2]==`Stop & stall[3]==`NoStop) begin
            flag <= 1'b1;
            inst_reg <= inst_sram_rdata;
        end
        else begin
            flag <= 1'b0;
            inst_reg <= 32'b0;
        end
    end
    always @ (posedge clk) begin
        if(stall[3]==1'b1 & stall[4]==1'b0 & inst_reg2 == 32'b0) begin
            flag2 <= 1'b1;
            inst_reg2 <= inst_sram_rdata;
        end
        else if(stall[3]==1'b0) begin
            flag2 <=1'b0;
            inst_reg2 <= 32'b0;
        end
    end  
    assign inst = flag ? inst_reg : flag2 ? inst_reg2 : inst_sram_rdata ;
    //
    wire br_e;
    wire [31:0] br_addr;
    wire rs_eq_rt;
    wire rs_not_eq_rt;
    wire rs_ge_z;
    wire rs_gt_z;
    wire rs_le_z;
    wire rs_lt_z;
    wire [31:0] pc_plus_4;
    assign pc_plus_4 = id_pc + 32'h4;
//    assign pc_plus_8 = id_pc + 32'h8;
    assign rs_eq_rt = (rdata11 == rdata22);
    assign rs_not_eq_rt = (rdata11 != rdata22);
    assign br_e = (inst_beq & rs_eq_rt) 
    | inst_jal 
    | inst_jr 
    | (inst_bne & rs_not_eq_rt)
    | inst_j 
    | (inst_bgez& rdata11[31]==1'b0)
    | (inst_bgtz&(rdata11!=32'b0)&&(rdata11[31]==1'b0))
    | (inst_blez&((rdata11[31]!=1'b0)||rdata11==32'b0))
    | (inst_bltz&(rdata11[31]!=1'b0))
    | (inst_bltzal&(rdata11[31]!=1'b0))
    | (inst_bgezal&(rdata11[31]==1'b0))
    |inst_jalr;
    
    assign br_addr = inst_beq ? (pc_plus_4 + {{14{inst[15]}},inst[15:0],2'b0}) : 
    inst_jal ? ({pc_plus_4[31:28], inst[25:0],2'b0}): 
    inst_jr ? rdata11: 
    inst_bne ? (pc_plus_4+{{14{inst[15]}},{inst[15:0],2'b00}}):
    inst_j ? ( {pc_plus_4[31:28], instr_index, 2'b00}) :
    inst_bgez ? (pc_plus_4 + {{14{inst[15]}},inst[15:0],2'b00}):
    inst_bgtz?(pc_plus_4 + {{14{inst[15]}},inst[15:0],2'b00}):
    inst_blez?(pc_plus_4 + {{14{inst[15]}},inst[15:0],2'b00}):
    inst_bltz?(pc_plus_4 + {{14{inst[15]}},inst[15:0],2'b00}):
    inst_bltzal?(pc_plus_4 + {{14{inst[15]}},inst[15:0],2'b00}):
    inst_bgezal?(pc_plus_4 + {{14{inst[15]}},inst[15:0],2'b00}):
    inst_jalr?(rdata11):
     32'b0;
                                                                                                                                                      
    assign br_bus = {
        br_e,
        br_addr
    };
    
    
    
    

endmodule