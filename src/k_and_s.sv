module k_and_s
import k_and_s_pkg::*;
(
    input  logic        rst_n,
    input  logic        clk,
    output logic        halt,
    output logic  [4:0] addr,
    input  logic [15:0] data_in,
    output logic [15:0] data_out,
    output logic        write_enable
);

  logic                    branch_s;
  logic                    pc_enable_s;
  logic                    ir_enable_s;
  logic                    addr_sel_s;
  logic                    c_sel_s;
  logic                    flags_reg_enable_s;
  decoded_instruction_type decoded_instruction_s;
  logic                    write_reg_enable_s;
  logic              [1:0] operation_s;
  logic                    zero_op_s;
  logic                    neg_op_s;
  logic                    unsigned_overflow_s;
  logic                    signed_overflow_s;

data_path datapath_i
(
    .rst_n(rst_n),
    .clk(clk),
    .branch(branch_s),
    .pc_enable(pc_enable_s),
    .ir_enable(ir_enable_s),
    .addr_sel(addr_sel_s),
    .c_sel(c_sel_s),
    .operation(operation_s),
    .write_reg_enable(write_reg_enable_s),
    .flags_reg_enable(flags_reg_enable_s),
    .decoded_instruction(decoded_instruction_s),
    .zero_op(zero_op_s),
    .neg_op(neg_op_s),
    .unsigned_overflow(unsigned_overflow_s),
    .signed_overflow(signed_overflow_s),
    .ram_addr(addr),
    .data_out(data_out),
    .data_in(data_in)
);

control_unit control_unit_i
(
    .rst_n(rst_n),
    .clk(clk),
    .branch(branch_s),
    .pc_enable(pc_enable_s),
    .ir_enable(ir_enable_s),
    .write_reg_enable(write_reg_enable_s),
    .addr_sel(addr_sel_s),
    .c_sel(c_sel_s),
    .operation(operation_s),
    .flags_reg_enable(flags_reg_enable_s),
    .decoded_instruction(decoded_instruction_s),
    .zero_op(zero_op_s),
    .neg_op(neg_op_s),
    .unsigned_overflow(unsigned_overflow_s),
    .signed_overflow(signed_overflow_s),
    .ram_write_enable(write_enable),
    .halt(halt)
);



endmodule : k_and_s
