`include "lib/defines.vh"
module MEM(
    input wire clk,
    input wire rst,
    input wire [`StallBus-1:0] stall,
    input wire [`EX_TO_MEM_WD-1+64+1+2+32:0] ex_to_mem_bus,
    input wire [31:0] data_sram_rdata,
    output wire [`MEM_TO_WB_WD-1+64+1+2:0] mem_to_wb_bus,
    output wire [37+64+1+2:0] mem_to_id_bus
);

    reg [`EX_TO_MEM_WD-1+64+1+2+32:0] ex_to_mem_bus_r;

    always @ (posedge clk) begin
        if (rst) begin
            ex_to_mem_bus_r <= `EX_TO_MEM_WD+64+1+2+32'b0;
        end
        else if (stall[3]==`Stop && stall[4]==`NoStop) begin
            ex_to_mem_bus_r <= `EX_TO_MEM_WD+64+1+2+32'b0;
        end
        else if (stall[3]==`NoStop) begin
            ex_to_mem_bus_r <= ex_to_mem_bus;
        end
    end

    wire [31:0] mem_pc;
    wire data_ram_en;
    wire [3:0] data_ram_wen;
    wire sel_rf_res;
    wire rf_we;
    wire [4:0] rf_waddr;
    wire [31:0] rf_wdata;
    wire [31:0] ex_result;
    wire [31:0] mem_result;
    
    wire [63:0] div_result;
    wire div_flag;
    wire [1:0] mt_flag;
    wire [31:0] inst;
    assign {
        mt_flag,
        div_flag,         // 107
        div_result,      //106:76
        inst,
        mem_pc,         // 75:44
        data_ram_en,    // 43
        data_ram_wen,   // 42:39
        sel_rf_res,     // 38
        rf_we,          // 37
        rf_waddr,       // 36:32 
        ex_result       // 31:0
    } =  ex_to_mem_bus_r;

     wire [31:0] data_sram_rdata2;
     assign data_sram_rdata2 = (inst[31:26]==6'b100000) ?  ( (ex_result[0] == 1'b0 & ex_result[1] == 1'b0) ? {{24{data_sram_rdata[7]}},data_sram_rdata[7:0]} 
     : (ex_result[0] == 1'b1 & ex_result[1] == 1'b0) ? {{24{data_sram_rdata[15]}},data_sram_rdata[15:8]} 
     : (ex_result[0] == 1'b0 & ex_result[1] == 1'b1) ? {{24{data_sram_rdata[23]}},data_sram_rdata[23:16]}
     : (ex_result[0] == 1'b1 & ex_result[1] == 1'b1) ? {{24{data_sram_rdata[31]}},data_sram_rdata[31:24]}
     : data_sram_rdata ) 
     : (inst[31:26]==6'b100100) ? ( (ex_result[0] == 1'b0 & ex_result[1] == 1'b0) ? {24'b0,data_sram_rdata[7:0]} 
     : (ex_result[0] == 1'b1 & ex_result[1] == 1'b0) ? {24'b0,data_sram_rdata[15:8]} 
     : (ex_result[0] == 1'b0 & ex_result[1] == 1'b1) ? {24'b0,data_sram_rdata[23:16]}
     : (ex_result[0] == 1'b1 & ex_result[1] == 1'b1) ? {24'b0,data_sram_rdata[31:24]}
     : data_sram_rdata )
     : (inst[31:26]==6'b100001) ? ( ex_result[1] == 1'b0 ? {{16{data_sram_rdata[15]}},data_sram_rdata[15:0]} 
     : ( ex_result[1] == 1'b1) ? {{16{data_sram_rdata[31]}},data_sram_rdata[31:16]} 
     : data_sram_rdata )
     : (inst[31:26]==6'b100101) ? ( ex_result[1] == 1'b0 ? {16'b0,data_sram_rdata[15:0]} 
     : ( ex_result[1] == 1'b1) ? {16'b0,data_sram_rdata[31:16]}
     : data_sram_rdata )  
     :data_sram_rdata;
     assign rf_wdata = (data_ram_wen==4'b0000 & data_ram_en==1'b1) ? data_sram_rdata2 : sel_rf_res ? mem_result : ex_result;

    assign mem_to_wb_bus = {
        mt_flag,
        div_flag,          
        div_result,      
        mem_pc,     // 69:38
        rf_we,      // 37
        rf_waddr,   // 36:32
        rf_wdata    // 31:0
    };
    assign mem_to_id_bus={
        mt_flag,
        div_flag,         
        div_result, 
        rf_we,       
        rf_waddr,   
        rf_wdata    
    };



endmodule