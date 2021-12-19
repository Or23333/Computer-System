`include"defines.vh"

//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2021/12/13 18:49:12
// Design Name: 
// Module Name: mymul
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////


module mymul(
    input wire rst,
    input wire clk, 
    input wire signed_mul_i,
    input wire[31:0] opdata1_i,				//������
	input wire[31:0] opdata2_i,				//����
	input wire start_i,						//�Ƿ�ʼ�˷�����
    input wire annul_i,                     //�Ƿ�ȡ���˷����㣬1Ϊȡ��
	output reg[63:0] result_o,				//�˷�������
	output reg ready_o						//�˷������Ƿ����
);
    // three states, mul_free, mul_on, mul_end�� mul_by_zero
    reg [31:0] temp_op1;
    reg [31:0] temp_op2;
    reg [63:0] multiplicand;   //������ ÿ����������һλ x
    reg [31:0] multiplier;     //����   ÿ����������һλ y
    reg [63:0] product_temp;   //��ʱ���

    reg [5:0] cnt;      //if 32, mul stop
    reg [1:0] state;    //�����ĸ�״̬

    wire [63:0] partial_product; //���ֻ�

    assign partial_product = multiplier[0] ? multiplicand : {`ZeroWord, `ZeroWord};   //�жϳ������λ�Ƿ�Ϊ1

    always @ (posedge clk) begin
        if (rst) begin
            state <= `MulFree;
            ready_o <= `MulResultNotReady;
            result_o <= {`ZeroWord, `ZeroWord};
        end 
        else begin
            case (state)
                `MulFree: begin
                    if (start_i == `MulStart && annul_i == 1'b0) begin
                        if (opdata1_i == `ZeroWord || opdata2_i == `ZeroWord) begin   //�κβ�����Ϊ0��������MUL_BY_ZERO״̬
                            state <= `MulByZero;
                        end 
                        else begin
                            state <= `MulOn;
                            cnt <= 6'b000000;
                            if (signed_mul_i == 1'b1 && opdata1_i[31] == 1'b1) begin    //op1Ϊ������ȡ����
                                temp_op1 = ~opdata1_i + 1;
                            end 
                            else begin
                                temp_op1 = opdata1_i;
                            end
                            if (signed_mul_i == 1'b1 && opdata2_i[31] == 1'b1) begin    //op2Ϊ������ȡ����
                                temp_op2 = ~opdata2_i + 1;
                            end 
                            else begin
                                temp_op2 = opdata2_i;
                            end
                            multiplicand <= {32'b0, temp_op1};
                            multiplier <= temp_op2;
                            product_temp <= {`ZeroWord, `ZeroWord};
                        end
                    end 
                    else begin
                        ready_o <= `DivResultNotReady;
                        result_o <= {`ZeroWord, `ZeroWord};
                    end
                end

                `MulByZero: begin
                    product_temp <= {`ZeroWord, `ZeroWord};    //��һ����������0�� �����Ϊ0
                    state <= `MulEnd;
                end

                `MulOn: begin
                    if (annul_i == 1'b0) begin
                        if (cnt != 6'b100000) begin
                            multiplicand <= {multiplicand[62:0], 1'b0};        //����������
                            multiplier <= {1'b0, multiplier[31:1]};            //��������
                            product_temp <= product_temp + partial_product;    //���
                            cnt <= cnt + 1;
                        end 
                        else begin   //������������ԭ��������Ϊһ��һ����ȡ����
                            if ((signed_mul_i == 1'b1) && ((opdata1_i[31] ^ opdata2_i[31]) == 1'b1)) begin
                                product_temp <= ~product_temp + 1;
                            end
                            state <= `MulEnd;
                            cnt <= 6'b000000;
                        end
                    end else begin
                        state <= `MulFree;
                    end
                end

                `MulEnd: begin
                    result_o <= product_temp;
                    ready_o <= `MulResultReady;
                    if (start_i == `MulStop) begin
                        state <= `MulFree;
                        ready_o <= `MulResultNotReady;
                        result_o <= {`ZeroWord, `ZeroWord};
                    end
                end
            endcase
        end
    end

endmodule