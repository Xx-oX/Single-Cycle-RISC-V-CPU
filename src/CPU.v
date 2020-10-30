// Please include verilog file if you write module in other file
module CPU(
    input             clk,
    input             rst,
    input      [31:0] data_out,
    input      [31:0] instr_out,
    output reg        instr_read,
    output reg        data_read,
    output reg [31:0] instr_addr,
    output reg [31:0] data_addr,
    output reg [3:0]  data_write,
    output reg [31:0] data_in
);

reg [31:0] Reg[0:31];
reg [31:0] pc;
reg [31:0] imm;
reg [6:0] op;
reg [24:0] noopinst;

reg [4:0] rs1, rs2, rd, shamt;
reg [6:0] f7;
reg [2:0] f3; 
reg [11:0] imm12;
reg [19:0] imm20;
reg tmp;

integer i;

reg [31:0] ins;
reg [3:0] state;
reg [15:0] printTime = 16'd0;

parameter MaxPrintTime = 16'd27000;
parameter MinPrintTime = 16'd22000;
initial begin
	//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("initial");
	ins <= 32'h0;
    state <= 4'd0;
	for(i=0; i<32; i=i+1)
		Reg[i] <= 32'h0;
    pc <= 32'h0;
	imm <= 32'h0;

    instr_read <= 1'b0;
    data_read <= 1'b0;
    data_write <= 4'h0;
    instr_addr <= 32'h0;
    data_addr <= 32'h0;
    data_in <= 32'h0;	
end

always @(posedge clk) begin
	if(rst == 1'b1) begin
        //if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("reset");
		ins <= 32'h0;
        state <= 4'd0;
		for(i=0; i<32; i=i+1)
			Reg[i] <= 32'h0;
        pc <= 32'h0;
		imm <= 32'h0;

        instr_read <= 1'b0;
        data_read <= 1'b0;
        data_write <= 4'h0;
        instr_addr <= 32'h0;
        data_addr <= 32'h0;
        data_in <= 32'h0;
    end
    else begin
        case(state)
            4'd0:
            begin
				Reg[0] <= 1'b0;
                //if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("state%d", state);
                instr_read <= 1;
				instr_addr <= pc;
				state <= 4'd1;

                //if(printTime < MaxPrintTime && printTime > MinPrintTime) 
					//$display("pc:%h, imm:%h, r[%h]:%h, r[%h]:%h, r[%h]:%h", pc, imm, rs1, Reg[rs1], rs2, Reg[rs2], rd, Reg[rd]);
			end
            4'd1:
            begin
				Reg[0] <= 1'b0;
                //if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("state%d", state);
                instr_read <= 0;
				state <= 4'd2;
				//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("pc:%h, ins:%b, instr_out:%b", pc, ins, instr_out);
            end
            4'd2:
            begin
				Reg[0] <= 1'b0;
				//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("state%d", state);
                ins <= instr_out;
				{noopinst, op} <= instr_out;
				state <= 4'd3;
				//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("pc:%h, ins:%b, instr_out:%b", pc, ins, instr_out);
            end
            4'd3:
            begin
				Reg[0] <= 1'b0;
				//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("state%d", state);
				//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("pc:%h, ins:%b, instr_out:%b", pc, ins, instr_out);
				/*decode*/
				//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("op:%b", op);
				//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("@%h:%b", pc, ins);
				case(op)
					//R-type
					7'b0110011:
					begin
						{f7, rs2, rs1, f3, rd} <= noopinst;
						state <= 4'd4;
						pc <= pc + 32'd4;
					end
					//I-type (load)
					7'b0000011:
					begin
						{imm12, rs1, f3, rd} <= noopinst;
						imm <= {{20{ins[31]}}, ins[31:20]};
						state <= 4'd5;
						pc <= pc + 32'd4;
					end
					//I-type (arithmetic)
					7'b0010011:
					begin
						{imm12, rs1, f3, rd} <= noopinst;
						imm <= {{20{ins[31]}}, ins[31:20]};
						shamt <= ins[24:20];
						state <= 4'd5;
						pc <= pc + 32'd4;
					end
					//I-type (JALR)
					7'b1100111:
					begin
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("JALR");
						{imm12, rs1, f3, rd} <= noopinst;
						imm <= {{20{ins[31]}}, ins[31:20]};
						state <= 4'd5;
					end
					//S-type
					7'b0100011:
					begin
						//borrow sign : imm[11:5] -> f7, imm[4:0] -> rd
						{f7, rs2, rs1, f3, rd} <= noopinst;
						imm <= {{20{ins[31]}}, ins[31:25], ins[11:7]};
						state <= 4'd6;
						pc <= pc + 32'd4;
					end
					//B-type
					7'b1100011:
					begin
						//borrow sign : imm[12|10:5] -> f7, imm[4:1|11] -> rd
						{f7, rs2, rs1, f3, rd} <= noopinst;
						imm <= {{19{ins[31]}}, ins[31], ins[7], ins[30:25], ins[11:8], 1'b0};
						state <= 4'd7;
					end
					//U-type (AUIPC)
					7'b0010111:
					begin
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("AUIPC");
						{imm20, rd} <= noopinst;
						imm <= {ins[31:12], 12'b0};
						state <= 4'd8;
					end
					//U-type (LUI)
					7'b0110111:
					begin
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("LUI");
						{imm20, rd} <= noopinst;
						imm <= {ins[31:12], 12'b0};
						state <= 4'd8;
					end
					//J-type (JAL)
					7'b1101111:
					begin
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("JAL");
						{imm20, rd} <= noopinst;
						imm <= {{11{ins[31]}}, ins[31], ins[19:12], ins[20], ins[30:21], 1'b0};
						state <= 4'd9;
					end
				endcase	
			end
			4'd4://R-Type
			begin
				Reg[0] <= 1'b0;
				//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("state%d", state);
				case({f7, f3})
					//ADD
					10'b0000000000:
					begin
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("ADD r%d r%d r%d = %h", rd, rs1, rs2, Reg[rd]);
						Reg[rd] <= Reg[rs1] + Reg[rs2];
					end
					//SUB
					10'b0100000000:
					begin
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("SUB r%d r%d r%d = %h", rd, rs1, rs2, Reg[rd]);
						Reg[rd] <= Reg[rs1] - Reg[rs2];
					end
					//SLL
					10'b0000000001:
					begin
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("SLL r%d r%d r%d = %h", rd, rs1, rs2, Reg[rd]);
						Reg[rd] <= $unsigned(Reg[rs1]) << Reg[rs2][4:0];
					end
					//SLT
					10'b0000000010:
					begin
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("SLT r%d r%d r%d = %h", rd, rs1, rs2, Reg[rd]);
						Reg[rd] <= $signed(Reg[rs1]) < $signed(Reg[rs2])? 32'd1 : 32'd0;
					end
					//SLTU
					10'b0000000011:
					begin
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("SLTU r%d r%d r%d = %h", rd, rs1, rs2, Reg[rd]);
						Reg[rd] <= $unsigned(Reg[rs1]) < $unsigned(Reg[rs2])? 32'd1 : 32'd0;
					end
					//XOR
					10'b0000000100:
					begin
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("XOR r%d r%d r%d = %h", rd, rs1, rs2, Reg[rd]);
						Reg[rd] <= Reg[rs1] ^ Reg[rs2];
					end
					//SRL
					10'b0000000101:
					begin
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("SRL r%d r%d r%d = %h", rd, rs1, rs2, Reg[rd]);
						Reg[rd] <= $unsigned(Reg[rs1]) >> Reg[rs2][4:0];
					end
					//SRA
					10'b0100000101:
					begin
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("SRA r%d r%d r%d = %h", rd, rs1, rs2, Reg[rd]);
						Reg[rd] <= $signed(Reg[rs1]) >>> Reg[rs2][4:0];
					end
					//OR
					10'b0000000110:
					begin
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("OR r%d r%d r%d = %h", rd, rs1, rs2, Reg[rd]);
						Reg[rd] <= Reg[rs1] | Reg[rs2];
					end
					//AND
					10'b0000000111:
					begin
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("AND r%d r%d r%d = %h", rd, rs1, rs2, Reg[rd]);
						Reg[rd] <= Reg[rs1] & Reg[rs2];
					end
				endcase
				state <= 4'd0;
			end
			4'd5://I-Type
			begin
				Reg[0] <= 1'b0;
				//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("state%d", state);
				//Load
				if(op == 7'b0000011)
				begin
					//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("load in state 5 %h + %h", Reg[rs1], imm);
					data_addr <= Reg[rs1] + imm;
					data_read <= 1'b1;
					state <= 4'd11;
				end
				//Arithmetic
				else if(op == 7'b0010011)
				begin
					case(f3)
						//ADDI
						3'b000:
						begin
							//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("ADDI r%d r%d %h = %h", rd, rs1, imm, Reg[rd]);
							//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("r%d = r%d(%d) + %d = %d", rd, rs1, Reg[rs1], imm, Reg[rd]);
							Reg[rd] <= Reg[rs1] + imm;
						end
						//SLTI
						3'b010:
						begin 
							//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("SLTI r%d r%d %h = %h", rd, rs1, imm, Reg[rd]);
							Reg[rd] <= $signed(Reg[rs1]) < $signed(imm)? 32'd1 : 32'd0;
						end
						//SLTIU
						3'b011:
						begin 
							//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("SLTIU r%d r%d %h = %h", rd, rs1, imm, Reg[rd]);
							Reg[rd] <= $unsigned(Reg[rs1]) < $unsigned(imm)? 32'd1 : 32'd0;
						end
						//XORI
						3'b100:
						begin 
							//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("XORI r%d r%d %h = %h", rd, rs1, imm, Reg[rd]);
							Reg[rd] <= Reg[rs1] ^ imm;
						end
						//ORI
						3'b110:
						begin 
							//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("ORI r%d r%d %h = %h", rd, rs1, imm, Reg[rd]);
							Reg[rd] <= Reg[rs1] | imm;
						end
						//ANDI
						3'b111:
						begin 
							//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("ANDI r%d r%d %h = %h", rd, rs1, imm, Reg[rd]);
							Reg[rd] <= Reg[rs1] & imm;
						end
						//SLLI
						3'b001:
						begin 
							//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("SLLI r%d r%d %h = %h", rd, rs1, imm, Reg[rd]);
							Reg[rd] <= $unsigned(Reg[rs1]) << shamt;
						end
						//SRLI / SRAI
						3'b101:
						begin
							//SRLI
							if(imm12[10] == 1'b0)
							begin
								//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("SRLI r%d r%d %h = %h", rd, rs1, shamt, Reg[rd]);
								Reg[rd] <= $unsigned(Reg[rs1]) >> shamt;
							end
							//SRAI
							else if(imm12[10] == 1'b1)
							begin
								//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("SRAI r%d r%d %h = %h", rd, rs1, shamt, Reg[rd]);
								Reg[rd] <= $signed(Reg[rs1]) >>> shamt;
							end
						end
					endcase
					state <= 4'd0;
				end	
				//JALR	
				else
				begin
					//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("JALR r%d r%d %h = rd:%h pc:%h", rd, rs1, imm, Reg[rd], pc);
					Reg[rd] <= pc + 32'h4;
					pc <= imm + Reg[rs1];
					state <= 4'd0;
				end
			end
			4'd6://S-Type
			begin
				Reg[0] <= 1'b0;
				//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("state%d", state);
				case(f3)
					//SW
					3'b010:
					begin
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("SW r%d r%d %h", rs1, rs2, imm);
						data_write <= 4'b1111;
					end
					//SB
					3'b000:
					begin 
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("SB r%d r%d %h", rs1, rs2, imm);
						data_write <= 4'b0001 << ((Reg[rs1]+imm) % 4);
					end
					//SH
					3'b001:
					begin 
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("SH r%d r%d %h", rs1, rs2, imm);
						data_write <= 4'b00011 << ((Reg[rs1]+imm) % 4); 
					end
				endcase
				data_addr <= Reg[rs1] + imm;//M[rs1+imm]
				data_in <= Reg[rs2] << (((Reg[rs1]+imm) % 4) << 3) ;//store
				state <= 4'd13;
			end
			4'd7://B-Type
			begin
				Reg[0] <= 1'b0;
				//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("state%d", state);	
				case(f3)
					//BEQ
					3'b000:
					begin 
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("BEQ r%d r%d %h = pc:%h", rs1, rs2, imm, pc);
						if(Reg[rs1] == Reg[rs2])
							pc <= pc + imm;
						else
							pc <= pc + 32'd4;
					end
					//BNE
					3'b001:
					begin 
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("BNE r%d r%d %h = pc:%h", rs1, rs2, imm, pc);
						if(Reg[rs1] != Reg[rs2])
							pc <= pc + imm;
						else
							pc <= pc + 32'd4;
					end
					//BLT
					3'b100:
					begin
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("BLT r%d r%d %h = pc:%h", rs1, rs2, imm, pc);
						if($signed(Reg[rs1]) < $signed(Reg[rs2]))
							pc <= pc + imm;
						else
							pc <= pc + 32'd4;
					end
					//BGE
					3'b101:
					begin
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("BGE r%d r%d %h = pc:%h", rs1, rs2, imm, pc);
						if($signed(Reg[rs1]) >= $signed(Reg[rs2]))
							pc <= pc + imm;
						else
							pc <= pc + 32'd4;
					end
					//BLTU
					3'b110:
					begin
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("BLTU r%d r%d %h = pc:%h", rs1, rs2, imm, pc);
						if($unsigned(Reg[rs1]) < $unsigned(Reg[rs2]))
							pc <= pc + imm;
						else
							pc <= pc + 32'd4;
					end
					//BGEU
					3'b111:
					begin
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("BGEU r%d r%d %h = pc:%h", rs1, rs2, imm, pc);
						if($unsigned(Reg[rs1]) >= $unsigned(Reg[rs2]))
							pc <= pc + imm;
						else
							pc <= pc + 32'd4;
					end
				endcase
				state <= 4'd0;
			end
			4'd8://U-Type
			begin
				Reg[0] <= 1'b0;
				//AUIPC
				if(op == 7'b0010111)
				begin
					//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("AUIPC r%d %h = rd:%h", rd, imm, Reg[rd]);
					Reg[rd] = pc + imm;
					pc = pc + 32'd4;
				end
				//LUI
				else
				begin
					//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("LUI r%d %h = rd:%h", rd, imm, Reg[rd]);
					Reg[rd] <= imm;
					pc <= pc + 32'd4;
				end
				state <= 4'd0;
			end
			4'd9://J-Type
			begin
				Reg[0] <= 1'b0;
				//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("JAL r%d %h = pc:%h", rd, imm, pc+imm);
				Reg[rd] = pc + 32'd4;
				pc = pc + imm;
				state <= 4'd0;
			end
			4'd10://Load
			begin
				Reg[0] <= 1'b0;
				//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("load in state 10 %h", data_out);
				case(f3)
					//LW
					3'b010:
					begin 
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("LW r%d %h = %h", rd, imm, Reg[rd]);
						Reg[rd] <= data_out;
					end
					//LB
					3'b000:
					begin
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("LB r%d %h = %h", rd, imm, Reg[rd]);
						Reg[rd] <= {{24{data_out[7]}}, data_out[7:0]};
					end
					//LH
					3'b001:
					begin 
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("LH r%d %h = %h", rd, imm, Reg[rd]);
						Reg[rd] <= {{16{data_out[15]}}, data_out[15:0]};
					end
					//LBU
					3'b100:
					begin
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("LBU r%d %h = %h", rd, imm, Reg[rd]);
						Reg[rd] <= {{24{1'b0}}, data_out[7:0]};
					end
					//LHU
					3'b101:
					begin
						//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("LHU r%d %h = %h", rd, imm, Reg[rd]);
						Reg[rd] <= {{16{1'b0}}, data_out[15:0]};
					end
				endcase
				state <= 4'd12;
			end
			4'd11://idle for load
			begin
				//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("load idle cycle @%h ", data_addr);
				data_read <= 1'b0;
				state <= 4'd10;
			end	
			4'd12://idle for printing
			begin
				//if(printTime < MaxPrintTime && printTime > MinPrintTime) $display("r[%h] = %h", rd, Reg[rd]);
				state <= 4'd0;
			end
			4'd13://idle for writing
			begin
				//if(printTime < MaxPrintTime && printTime > MinPrintTime) 
					//$display("save @%h r[%h] = %h", Reg[rs1] + imm, rs2, Reg[rs2]);
				data_write = 4'd0;
				state <= 4'd0;
			end
		endcase
		if(printTime < MaxPrintTime)
			printTime <= printTime + 1;
    end
end
endmodule
