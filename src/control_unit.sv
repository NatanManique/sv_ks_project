//control unit
module control_unit
import k_and_s_pkg::*;
(
    input  logic                    rst_n,
    input  logic                    clk,
    output logic                    branch,
    output logic                    pc_enable,
    output logic                    ir_enable,
    output logic                    write_reg_enable,
    output logic                    addr_sel,
    output logic                    c_sel,
    output logic              [1:0] operation,
    output logic                    flags_reg_enable,
    input  decoded_instruction_type decoded_instruction,
    input  logic                    zero_op,
    input  logic                    neg_op,
    input  logic                    unsigned_overflow,
    input  logic                    signed_overflow,
    output logic                    ram_write_enable,
    output logic                    halt
);

typedef enum 
{ 
    DECODIFICA
,   BUSCA_INSTR
,   REG_ESCREVE
,   REG_INSTR
,   LOAD1
,   STORE2
,   FIM_PROGRAMA
,   BRANCH_

} STATE_T;

STATE_T state;
STATE_T next_state;


always @(posedge clk or negedge rst_n) begin : mem_fsm
    if (!rst_n) begin
        state <= BUSCA_INSTR; 
    end
    else
        state <= next_state;
end : mem_fsm


always_comb begin : prox_estado
    
    branch              = 1'd0;
    pc_enable           = 1'd0;
    ir_enable           = 1'd0;
    write_reg_enable    = 1'd0;
    addr_sel            = 1'd0;
    c_sel               = 1'd0;
    operation           = 2'd00;
    flags_reg_enable    = 1'd0;
    ram_write_enable    = 1'd0;
    halt                = 1'd0;

    case (state)
        BUSCA_INSTR: begin    
            next_state = REG_INSTR;
           
        end
        REG_INSTR : begin
            next_state = DECODIFICA;
             ir_enable = 1'b1;
             pc_enable = 1'b1;
        end
        DECODIFICA: begin    
            next_state = BUSCA_INSTR;
            case(decoded_instruction)
                I_HALT: begin
                    next_state = FIM_PROGRAMA;        
                end
                I_LOAD: begin
                    next_state = LOAD1;
                    addr_sel   = 1'b1;
                end
                I_STORE: begin
                    next_state = STORE2;
                    addr_sel   = 1'b1;
                end
                I_MOVE: begin
                    next_state = REG_ESCREVE;                  
                end 
                I_ADD: begin
                    next_state = REG_ESCREVE;               
                end        
                I_SUB: begin
                    next_state = REG_ESCREVE;
                end        
                I_AND: begin
                    next_state = REG_ESCREVE;
                    end     
                I_OR:  begin
                    next_state = REG_ESCREVE;                   
                end    
                I_BRANCH:begin
                    next_state = BRANCH_;
                end         
                I_BZERO: begin
                    if (zero_op) begin
                       next_state = BRANCH_;
                    end
                    next_state = BUSCA_INSTR;
                end
                I_BNZERO: begin
                    if (!zero_op) begin
                        next_state = BRANCH_;
                    end
                    next_state = BUSCA_INSTR;
                end      
                I_BNEG: begin
                    if (neg_op) begin
                        next_state = BRANCH_;
                    end
                    next_state = BUSCA_INSTR;
                end        
                I_BNNEG: begin
                    if (!neg_op) begin
                        next_state = BRANCH_;
                    end
                    next_state = BUSCA_INSTR;
                end       
                I_BOV: begin
                    if(unsigned_overflow) begin
                            next_state = BRANCH_;
                        end
                        next_state = BUSCA_INSTR;
                end         
                I_BNOV: begin
                     if( signed_overflow) begin
                            next_state = BRANCH_;
                        end
                        next_state = BUSCA_INSTR;
                end
            endcase
        end
         LOAD1 : begin
            next_state       = BUSCA_INSTR;
            write_reg_enable = 1'b1;
            ir_enable        = 1'b0;
            c_sel            = 1'b0;
        end
        STORE2 : begin 
            next_state       = BUSCA_INSTR;
            ram_write_enable = 1'b1;
            ir_enable        = 1'b0;
            addr_sel         = 1'b1;
        end
        REG_ESCREVE: begin
            next_state       = BUSCA_INSTR;
            write_reg_enable = 1'b1;
            c_sel            = 1'b1;
            flags_reg_enable = 1'b1;

            case(decoded_instruction)
                I_SUB:   operation  = 2'b10;
                I_ADD:   operation  = 2'b01;
                I_AND:   operation  = 2'b11;  
                default: operation = 2'b00;
            endcase
        end
        BRANCH_: begin
             next_state = BUSCA_INSTR;
             branch     = 1'b1;
             pc_enable  = 1'b1;
        end
        FIM_PROGRAMA : begin
            next_state  = FIM_PROGRAMA;
            halt        = 1'b1;
        end
    endcase
end : prox_estado

endmodule : control_unit
