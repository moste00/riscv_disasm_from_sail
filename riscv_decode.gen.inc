#include <stdint.h>
#include "riscv_ast.gen.inc"



void decode(struct ast *tree, uint64_t binary_stream) {
    uint64_t op = 0xFFFFFFFFFFFFFFFF ;
    switch (binary_stream & 0x000000000000007F) {
      case 0x37: 
            op = RISCV_LUI ;
      break; 
      case 0x17: 
            op = RISCV_AUIPC ;
      break; 
 
    }
    if (op != 0xFFFFFFFFFFFFFFFF) { 
        uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
        uint64_t imm = (binary_stream & 0x00000000FFFFF000)>>12 ;
        tree->ast_node_type = UTYPE ;
        tree->ast_node.utype.imm = imm;
        tree->ast_node.utype.rd = rd;
        tree->ast_node.utype.op = op;
        return ;
    }
  if ((binary_stream & 0x000000000000007F == 0x6F)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t imm_7_0 = (binary_stream & 0x00000000000FF000)>>12 ;
      uint64_t imm_8 = (binary_stream & 0x0000000000100000)>>20 ;
      uint64_t imm_12_9 = (binary_stream & 0x0000000001E00000)>>21 ;
      uint64_t imm_18_13 = (binary_stream & 0x000000007E000000)>>25 ;
      uint64_t imm_19 = (binary_stream & 0x0000000080000000)>>31 ;
      tree->ast_node_type = RISCV_JAL ;
      tree->ast_node.riscv_jal.imm = (imm_19 << 23) | (imm_7_0 << 15) | (imm_8 << 14) | (imm_18_13 << 8) | (imm_12_9 << 4) | (0x0 << 0);
      tree->ast_node.riscv_jal.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x67) && ((binary_stream & 0x0000000000007000)>>12 == 0x0)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t imm = (binary_stream & 0x00000000FFF00000)>>20 ;
      tree->ast_node_type = RISCV_JALR ;
      tree->ast_node.riscv_jalr.imm = imm;
      tree->ast_node.riscv_jalr.rs1 = rs1;
      tree->ast_node.riscv_jalr.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x63)) { 
      uint64_t op = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                op = RISCV_BGEU ;
        break; 
        case 0x5: 
                op = RISCV_BGE ;
        break; 
        case 0x1: 
                op = RISCV_BNE ;
        break; 
        case 0x4: 
                op = RISCV_BLT ;
        break; 
        case 0x6: 
                op = RISCV_BLTU ;
        break; 
        case 0x0: 
                op = RISCV_BEQ ;
        break; 
 
      }
      if (op != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t imm5_0 = (binary_stream & 0x0000000000000080)>>7 ;
          uint64_t imm5_4_1 = (binary_stream & 0x0000000000000F00)>>8 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t imm7_5_0 = (binary_stream & 0x000000007E000000)>>25 ;
          uint64_t imm7_6 = (binary_stream & 0x0000000080000000)>>31 ;
          tree->ast_node_type = BTYPE ;
          tree->ast_node.btype.imm = (imm7_6 << 15) | (imm5_0 << 14) | (imm7_5_0 << 8) | (imm5_4_1 << 4) | (0x0 << 0);
          tree->ast_node.btype.rs2 = rs2;
          tree->ast_node.btype.rs1 = rs1;
          tree->ast_node.btype.op = op;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x13)) { 
      uint64_t op = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                op = RISCV_ANDI ;
        break; 
        case 0x3: 
                op = RISCV_SLTIU ;
        break; 
        case 0x2: 
                op = RISCV_SLTI ;
        break; 
        case 0x6: 
                op = RISCV_ORI ;
        break; 
        case 0x4: 
                op = RISCV_XORI ;
        break; 
        case 0x0: 
                op = RISCV_ADDI ;
        break; 
 
      }
      if (op != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t imm = (binary_stream & 0x00000000FFF00000)>>20 ;
          tree->ast_node_type = ITYPE ;
          tree->ast_node.itype.imm = imm;
          tree->ast_node.itype.rs1 = rs1;
          tree->ast_node.itype.rd = rd;
          tree->ast_node.itype.op = op;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FC000000)>>26 == 0x00)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t shamt = (binary_stream & 0x0000000003F00000)>>20 ;
      tree->ast_node_type = SHIFTIOP ;
      tree->ast_node.shiftiop.shamt = shamt;
      tree->ast_node.shiftiop.rs1 = rs1;
      tree->ast_node.shiftiop.rd = rd;
      tree->ast_node.shiftiop.op = RISCV_SLLI;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FC000000)>>26 == 0x00)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t shamt = (binary_stream & 0x0000000003F00000)>>20 ;
      tree->ast_node_type = SHIFTIOP ;
      tree->ast_node.shiftiop.shamt = shamt;
      tree->ast_node.shiftiop.rs1 = rs1;
      tree->ast_node.shiftiop.rd = rd;
      tree->ast_node.shiftiop.op = RISCV_SRLI;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FC000000)>>26 == 0x10)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t shamt = (binary_stream & 0x0000000003F00000)>>20 ;
      tree->ast_node_type = SHIFTIOP ;
      tree->ast_node.shiftiop.shamt = shamt;
      tree->ast_node.shiftiop.rs1 = rs1;
      tree->ast_node.shiftiop.rd = rd;
      tree->ast_node.shiftiop.op = RISCV_SRAI;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000FE000000)>>25 == 0x00)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RTYPE ;
      tree->ast_node.rtype.rs2 = rs2;
      tree->ast_node.rtype.rs1 = rs1;
      tree->ast_node.rtype.rd = rd;
      tree->ast_node.rtype.op = RISCV_ADD;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000FE000000)>>25 == 0x00)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RTYPE ;
      tree->ast_node.rtype.rs2 = rs2;
      tree->ast_node.rtype.rs1 = rs1;
      tree->ast_node.rtype.rd = rd;
      tree->ast_node.rtype.op = RISCV_SLT;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x3) && ((binary_stream & 0x00000000FE000000)>>25 == 0x00)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RTYPE ;
      tree->ast_node.rtype.rs2 = rs2;
      tree->ast_node.rtype.rs1 = rs1;
      tree->ast_node.rtype.rd = rd;
      tree->ast_node.rtype.op = RISCV_SLTU;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x7) && ((binary_stream & 0x00000000FE000000)>>25 == 0x00)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RTYPE ;
      tree->ast_node.rtype.rs2 = rs2;
      tree->ast_node.rtype.rs1 = rs1;
      tree->ast_node.rtype.rd = rd;
      tree->ast_node.rtype.op = RISCV_AND;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x6) && ((binary_stream & 0x00000000FE000000)>>25 == 0x00)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RTYPE ;
      tree->ast_node.rtype.rs2 = rs2;
      tree->ast_node.rtype.rs1 = rs1;
      tree->ast_node.rtype.rd = rd;
      tree->ast_node.rtype.op = RISCV_OR;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x4) && ((binary_stream & 0x00000000FE000000)>>25 == 0x00)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RTYPE ;
      tree->ast_node.rtype.rs2 = rs2;
      tree->ast_node.rtype.rs1 = rs1;
      tree->ast_node.rtype.rd = rd;
      tree->ast_node.rtype.op = RISCV_XOR;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FE000000)>>25 == 0x00)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RTYPE ;
      tree->ast_node.rtype.rs2 = rs2;
      tree->ast_node.rtype.rs1 = rs1;
      tree->ast_node.rtype.rd = rd;
      tree->ast_node.rtype.op = RISCV_SLL;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FE000000)>>25 == 0x00)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RTYPE ;
      tree->ast_node.rtype.rs2 = rs2;
      tree->ast_node.rtype.rs1 = rs1;
      tree->ast_node.rtype.rd = rd;
      tree->ast_node.rtype.op = RISCV_SRL;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000FE000000)>>25 == 0x20)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RTYPE ;
      tree->ast_node.rtype.rs2 = rs2;
      tree->ast_node.rtype.rs1 = rs1;
      tree->ast_node.rtype.rd = rd;
      tree->ast_node.rtype.op = RISCV_SUB;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FE000000)>>25 == 0x20)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RTYPE ;
      tree->ast_node.rtype.rs2 = rs2;
      tree->ast_node.rtype.rs1 = rs1;
      tree->ast_node.rtype.rd = rd;
      tree->ast_node.rtype.op = RISCV_SRA;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x03)) { 
      uint64_t size = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000003000)>>12) {
        case 0x1: 
                size = HALF ;
        break; 
        case 0x3: 
                size = DOUBLE ;
        break; 
        case 0x2: 
                size = WORD ;
        break; 
        case 0x0: 
                size = BYTE ;
        break; 
 
      }
      if (size != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t is_unsigned = 0xFFFFFFFFFFFFFFFF ;
          switch ((binary_stream & 0x0000000000004000)>>14) {
            case 0x1: 
                        is_unsigned = true ;
            break; 
            case 0x0: 
                        is_unsigned = false ;
            break; 
 
          }
          if (is_unsigned != 0xFFFFFFFFFFFFFFFF) { 
              uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
              uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
              uint64_t imm = (binary_stream & 0x00000000FFF00000)>>20 ;
              tree->ast_node_type = LOAD ;
              tree->ast_node.load.imm = imm;
              tree->ast_node.load.rs1 = rs1;
              tree->ast_node.load.rd = rd;
              tree->ast_node.load.is_unsigned = is_unsigned;
              tree->ast_node.load.width = size;
              tree->ast_node.load.aq = 0;
              tree->ast_node.load.rl = 0;
              return ;
          }
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x23) && ((binary_stream & 0x0000000000004000)>>14 == 0x0)) { 
      uint64_t size = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000003000)>>12) {
        case 0x1: 
                size = HALF ;
        break; 
        case 0x3: 
                size = DOUBLE ;
        break; 
        case 0x2: 
                size = WORD ;
        break; 
        case 0x0: 
                size = BYTE ;
        break; 
 
      }
      if (size != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t imm5 = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t imm7 = (binary_stream & 0x00000000FE000000)>>25 ;
          tree->ast_node_type = STORE ;
          tree->ast_node.store.imm = (imm7 << 5) | (imm5 << 0);
          tree->ast_node.store.rs2 = rs2;
          tree->ast_node.store.rs1 = rs1;
          tree->ast_node.store.width = size;
          tree->ast_node.store.aq = 0;
          tree->ast_node.store.rl = 0;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x1B) && ((binary_stream & 0x0000000000007000)>>12 == 0x0)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t imm = (binary_stream & 0x00000000FFF00000)>>20 ;
      tree->ast_node_type = ADDIW ;
      tree->ast_node.addiw.imm = imm;
      tree->ast_node.addiw.rs1 = rs1;
      tree->ast_node.addiw.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x3B) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000FE000000)>>25 == 0x00)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RTYPEW ;
      tree->ast_node.rtypew.rs2 = rs2;
      tree->ast_node.rtypew.rs1 = rs1;
      tree->ast_node.rtypew.rd = rd;
      tree->ast_node.rtypew.op = RISCV_ADDW;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x3B) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000FE000000)>>25 == 0x20)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RTYPEW ;
      tree->ast_node.rtypew.rs2 = rs2;
      tree->ast_node.rtypew.rs1 = rs1;
      tree->ast_node.rtypew.rd = rd;
      tree->ast_node.rtypew.op = RISCV_SUBW;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x3B) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FE000000)>>25 == 0x00)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RTYPEW ;
      tree->ast_node.rtypew.rs2 = rs2;
      tree->ast_node.rtypew.rs1 = rs1;
      tree->ast_node.rtypew.rd = rd;
      tree->ast_node.rtypew.op = RISCV_SLLW;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x3B) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FE000000)>>25 == 0x00)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RTYPEW ;
      tree->ast_node.rtypew.rs2 = rs2;
      tree->ast_node.rtypew.rs1 = rs1;
      tree->ast_node.rtypew.rd = rd;
      tree->ast_node.rtypew.op = RISCV_SRLW;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x3B) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FE000000)>>25 == 0x20)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RTYPEW ;
      tree->ast_node.rtypew.rs2 = rs2;
      tree->ast_node.rtypew.rs1 = rs1;
      tree->ast_node.rtypew.rd = rd;
      tree->ast_node.rtypew.op = RISCV_SRAW;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x1B) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FE000000)>>25 == 0x00)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t shamt = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = SHIFTIWOP ;
      tree->ast_node.shiftiwop.shamt = shamt;
      tree->ast_node.shiftiwop.rs1 = rs1;
      tree->ast_node.shiftiwop.rd = rd;
      tree->ast_node.shiftiwop.op = RISCV_SLLIW;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x1B) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FE000000)>>25 == 0x00)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t shamt = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = SHIFTIWOP ;
      tree->ast_node.shiftiwop.shamt = shamt;
      tree->ast_node.shiftiwop.rs1 = rs1;
      tree->ast_node.shiftiwop.rd = rd;
      tree->ast_node.shiftiwop.op = RISCV_SRLIW;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x1B) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FE000000)>>25 == 0x20)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t shamt = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = SHIFTIWOP ;
      tree->ast_node.shiftiwop.shamt = shamt;
      tree->ast_node.shiftiwop.rs1 = rs1;
      tree->ast_node.shiftiwop.rd = rd;
      tree->ast_node.shiftiwop.op = RISCV_SRAIW;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x0F) && ((binary_stream & 0x0000000000000F80)>>7 == 0x00) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000000F8000)>>15 == 0x00) && ((binary_stream & 0x00000000F0000000)>>28 == 0x0)) { 
      uint64_t succ = (binary_stream & 0x0000000000F00000)>>20 ;
      uint64_t pred = (binary_stream & 0x000000000F000000)>>24 ;
      tree->ast_node_type = FENCE ;
      tree->ast_node.fence.pred = pred;
      tree->ast_node.fence.succ = succ;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x0F) && ((binary_stream & 0x0000000000000F80)>>7 == 0x00) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000000F8000)>>15 == 0x00) && ((binary_stream & 0x00000000F0000000)>>28 == 0x8)) { 
      uint64_t succ = (binary_stream & 0x0000000000F00000)>>20 ;
      uint64_t pred = (binary_stream & 0x000000000F000000)>>24 ;
      tree->ast_node_type = FENCE_TSO ;
      tree->ast_node.fence_tso.pred = pred;
      tree->ast_node.fence_tso.succ = succ;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x0F) && ((binary_stream & 0x0000000000000F80)>>7 == 0x00) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000000F8000)>>15 == 0x00) && ((binary_stream & 0x00000000FFF00000)>>20 == 0x000)) { 
      tree->ast_node_type = FENCEI ;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x73) && ((binary_stream & 0x0000000000000F80)>>7 == 0x00) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000000F8000)>>15 == 0x00) && ((binary_stream & 0x00000000FFF00000)>>20 == 0x000)) { 
      tree->ast_node_type = ECALL ;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x73) && ((binary_stream & 0x0000000000000F80)>>7 == 0x00) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000000F8000)>>15 == 0x00) && ((binary_stream & 0x0000000001F00000)>>20 == 0x02) && ((binary_stream & 0x00000000FE000000)>>25 == 0x18)) { 
      tree->ast_node_type = MRET ;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x73) && ((binary_stream & 0x0000000000000F80)>>7 == 0x00) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000000F8000)>>15 == 0x00) && ((binary_stream & 0x0000000001F00000)>>20 == 0x02) && ((binary_stream & 0x00000000FE000000)>>25 == 0x08)) { 
      tree->ast_node_type = SRET ;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x73) && ((binary_stream & 0x0000000000000F80)>>7 == 0x00) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000000F8000)>>15 == 0x00) && ((binary_stream & 0x00000000FFF00000)>>20 == 0x001)) { 
      tree->ast_node_type = EBREAK ;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x73) && ((binary_stream & 0x0000000000000F80)>>7 == 0x00) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000000F8000)>>15 == 0x00) && ((binary_stream & 0x00000000FFF00000)>>20 == 0x105)) { 
      tree->ast_node_type = WFI ;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x73) && ((binary_stream & 0x0000000000000F80)>>7 == 0x00) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000FE000000)>>25 == 0x09)) { 
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = SFENCE_VMA ;
      tree->ast_node.sfence_vma.rs1 = rs1;
      tree->ast_node.sfence_vma.rs2 = rs2;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x2F) && ((binary_stream & 0x0000000000004000)>>14 == 0x0) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000F8000000)>>27 == 0x02)) { 
      uint64_t size = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000003000)>>12) {
        case 0x1: 
                size = HALF ;
        break; 
        case 0x3: 
                size = DOUBLE ;
        break; 
        case 0x2: 
                size = WORD ;
        break; 
        case 0x0: 
                size = BYTE ;
        break; 
 
      }
      if (size != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rl = 0xFFFFFFFFFFFFFFFF ;
          switch ((binary_stream & 0x0000000002000000)>>25) {
            case 0x1: 
                        rl = true ;
            break; 
            case 0x0: 
                        rl = false ;
            break; 
 
          }
          if (rl != 0xFFFFFFFFFFFFFFFF) { 
              uint64_t aq = 0xFFFFFFFFFFFFFFFF ;
              switch ((binary_stream & 0x0000000004000000)>>26) {
                case 0x1: 
                                aq = true ;
                break; 
                case 0x0: 
                                aq = false ;
                break; 
 
              }
              if (aq != 0xFFFFFFFFFFFFFFFF) { 
                  uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
                  uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
                  tree->ast_node_type = LOADRES ;
                  tree->ast_node.loadres.aq = aq;
                  tree->ast_node.loadres.rl = rl;
                  tree->ast_node.loadres.rs1 = rs1;
                  tree->ast_node.loadres.width = size;
                  tree->ast_node.loadres.rd = rd;
                  return ;
              }
          }
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x2F) && ((binary_stream & 0x0000000000004000)>>14 == 0x0) && ((binary_stream & 0x00000000F8000000)>>27 == 0x03)) { 
      uint64_t size = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000003000)>>12) {
        case 0x1: 
                size = HALF ;
        break; 
        case 0x3: 
                size = DOUBLE ;
        break; 
        case 0x2: 
                size = WORD ;
        break; 
        case 0x0: 
                size = BYTE ;
        break; 
 
      }
      if (size != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rl = 0xFFFFFFFFFFFFFFFF ;
          switch ((binary_stream & 0x0000000002000000)>>25) {
            case 0x1: 
                        rl = true ;
            break; 
            case 0x0: 
                        rl = false ;
            break; 
 
          }
          if (rl != 0xFFFFFFFFFFFFFFFF) { 
              uint64_t aq = 0xFFFFFFFFFFFFFFFF ;
              switch ((binary_stream & 0x0000000004000000)>>26) {
                case 0x1: 
                                aq = true ;
                break; 
                case 0x0: 
                                aq = false ;
                break; 
 
              }
              if (aq != 0xFFFFFFFFFFFFFFFF) { 
                  uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
                  uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
                  uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
                  tree->ast_node_type = STORECON ;
                  tree->ast_node.storecon.aq = aq;
                  tree->ast_node.storecon.rl = rl;
                  tree->ast_node.storecon.rs2 = rs2;
                  tree->ast_node.storecon.rs1 = rs1;
                  tree->ast_node.storecon.width = size;
                  tree->ast_node.storecon.rd = rd;
                  return ;
              }
          }
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x2F) && ((binary_stream & 0x0000000000004000)>>14 == 0x0)) { 
      uint64_t size = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000003000)>>12) {
        case 0x1: 
                size = HALF ;
        break; 
        case 0x3: 
                size = DOUBLE ;
        break; 
        case 0x2: 
                size = WORD ;
        break; 
        case 0x0: 
                size = BYTE ;
        break; 
 
      }
      if (size != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rl = 0xFFFFFFFFFFFFFFFF ;
          switch ((binary_stream & 0x0000000002000000)>>25) {
            case 0x1: 
                        rl = true ;
            break; 
            case 0x0: 
                        rl = false ;
            break; 
 
          }
          if (rl != 0xFFFFFFFFFFFFFFFF) { 
              uint64_t aq = 0xFFFFFFFFFFFFFFFF ;
              switch ((binary_stream & 0x0000000004000000)>>26) {
                case 0x1: 
                                aq = true ;
                break; 
                case 0x0: 
                                aq = false ;
                break; 
 
              }
              if (aq != 0xFFFFFFFFFFFFFFFF) { 
                  uint64_t op = 0xFFFFFFFFFFFFFFFF ;
                  switch ((binary_stream & 0x00000000F8000000)>>27) {
                    case 0x08: 
                                        op = AMOOR ;
                    break; 
                    case 0x01: 
                                        op = AMOSWAP ;
                    break; 
                    case 0x04: 
                                        op = AMOXOR ;
                    break; 
                    case 0x10: 
                                        op = AMOMIN ;
                    break; 
                    case 0x1C: 
                                        op = AMOMAXU ;
                    break; 
                    case 0x14: 
                                        op = AMOMAX ;
                    break; 
                    case 0x00: 
                                        op = AMOADD ;
                    break; 
                    case 0x0C: 
                                        op = AMOAND ;
                    break; 
                    case 0x18: 
                                        op = AMOMINU ;
                    break; 
 
                  }
                  if (op != 0xFFFFFFFFFFFFFFFF) { 
                      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
                      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
                      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
                      tree->ast_node_type = AMO ;
                      tree->ast_node.amo.op = op;
                      tree->ast_node.amo.aq = aq;
                      tree->ast_node.amo.rl = rl;
                      tree->ast_node.amo.rs2 = rs2;
                      tree->ast_node.amo.rs1 = rs1;
                      tree->ast_node.amo.width = size;
                      tree->ast_node.amo.rd = rd;
                      return ;
                  }
              }
          }
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x00000000FE000000)>>25 == 0x01)) { 
      struct mul_op mul_op ;
      uint8_t mul_op_is_valid = 0 ;
      switch ((binary_stream & 0x0000000000007000)>>12) { 
        case 0x1: 
        mul_op.high = 1;
        mul_op.signed_rs1 = 1;
        mul_op.signed_rs2 = 1;
        mul_op_is_valid = 1 ; 
        break; 

        case 0x3: 
        mul_op.high = 1;
        mul_op.signed_rs1 = 0;
        mul_op.signed_rs2 = 0;
        mul_op_is_valid = 1 ; 
        break; 

        case 0x2: 
        mul_op.high = 1;
        mul_op.signed_rs1 = 1;
        mul_op.signed_rs2 = 0;
        mul_op_is_valid = 1 ; 
        break; 

        case 0x0: 
        mul_op.high = 0;
        mul_op.signed_rs1 = 1;
        mul_op.signed_rs2 = 1;
        mul_op_is_valid = 1 ; 
        break; 
      }
      if (mul_op_is_valid == 1) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = MUL ;
          tree->ast_node.mul.rs2 = rs2;
          tree->ast_node.mul.rs1 = rs1;
          tree->ast_node.mul.rd = rd;
          tree->ast_node.mul.mul_op = mul_op;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000006000)>>13 == 0x2) && ((binary_stream & 0x00000000FE000000)>>25 == 0x01)) { 
      uint64_t s = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000001000)>>12) {
        case 0x1: 
                s = false ;
        break; 
        case 0x0: 
                s = true ;
        break; 
 
      }
      if (s != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = DIV ;
          tree->ast_node.div.rs2 = rs2;
          tree->ast_node.div.rs1 = rs1;
          tree->ast_node.div.rd = rd;
          tree->ast_node.div.s = s;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000006000)>>13 == 0x3) && ((binary_stream & 0x00000000FE000000)>>25 == 0x01)) { 
      uint64_t s = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000001000)>>12) {
        case 0x1: 
                s = false ;
        break; 
        case 0x0: 
                s = true ;
        break; 
 
      }
      if (s != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = REM ;
          tree->ast_node.rem.rs2 = rs2;
          tree->ast_node.rem.rs1 = rs1;
          tree->ast_node.rem.rd = rd;
          tree->ast_node.rem.s = s;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x3B) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000FE000000)>>25 == 0x01)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = MULW ;
      tree->ast_node.mulw.rs2 = rs2;
      tree->ast_node.mulw.rs1 = rs1;
      tree->ast_node.mulw.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x3B) && ((binary_stream & 0x0000000000006000)>>13 == 0x2) && ((binary_stream & 0x00000000FE000000)>>25 == 0x01)) { 
      uint64_t s = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000001000)>>12) {
        case 0x1: 
                s = false ;
        break; 
        case 0x0: 
                s = true ;
        break; 
 
      }
      if (s != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = DIVW ;
          tree->ast_node.divw.rs2 = rs2;
          tree->ast_node.divw.rs1 = rs1;
          tree->ast_node.divw.rd = rd;
          tree->ast_node.divw.s = s;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x3B) && ((binary_stream & 0x0000000000006000)>>13 == 0x3) && ((binary_stream & 0x00000000FE000000)>>25 == 0x01)) { 
      uint64_t s = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000001000)>>12) {
        case 0x1: 
                s = false ;
        break; 
        case 0x0: 
                s = true ;
        break; 
 
      }
      if (s != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = REMW ;
          tree->ast_node.remw.rs2 = rs2;
          tree->ast_node.remw.rs1 = rs1;
          tree->ast_node.remw.rd = rd;
          tree->ast_node.remw.s = s;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x73)) { 
      uint64_t op = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000003000)>>12) {
        case 0x1: 
                op = CSRRW ;
        break; 
        case 0x3: 
                op = CSRRC ;
        break; 
        case 0x2: 
                op = CSRRS ;
        break; 
 
      }
      if (op != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t is_imm = 0xFFFFFFFFFFFFFFFF ;
          switch ((binary_stream & 0x0000000000004000)>>14) {
            case 0x1: 
                        is_imm = true ;
            break; 
            case 0x0: 
                        is_imm = false ;
            break; 
 
          }
          if (is_imm != 0xFFFFFFFFFFFFFFFF) { 
              uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
              uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
              uint64_t csr = (binary_stream & 0x00000000FFF00000)>>20 ;
              tree->ast_node_type = CSR ;
              tree->ast_node.csr.csr = csr;
              tree->ast_node.csr.rs1 = rs1;
              tree->ast_node.csr.rd = rd;
              tree->ast_node.csr.is_imm = is_imm;
              tree->ast_node.csr.op = op;
              return ;
          }
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x73) && ((binary_stream & 0x0000000000000F80)>>7 == 0x00) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000000F8000)>>15 == 0x00) && ((binary_stream & 0x0000000001F00000)>>20 == 0x02) && ((binary_stream & 0x00000000FE000000)>>25 == 0x00)) { 
      tree->ast_node_type = URET ;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x0F) && ((binary_stream & 0x0000000000007000)>>12 == 0x0)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t succ = (binary_stream & 0x0000000000F00000)>>20 ;
      uint64_t pred = (binary_stream & 0x000000000F000000)>>24 ;
      uint64_t fm = (binary_stream & 0x00000000F0000000)>>28 ;
      tree->ast_node_type = FENCE_RESERVED ;
      tree->ast_node.fence_reserved.fm = fm;
      tree->ast_node.fence_reserved.pred = pred;
      tree->ast_node.fence_reserved.succ = succ;
      tree->ast_node.fence_reserved.rs = rs;
      tree->ast_node.fence_reserved.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x0F) && ((binary_stream & 0x0000000000007000)>>12 == 0x1)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t imm = (binary_stream & 0x00000000FFF00000)>>20 ;
      tree->ast_node_type = FENCEI_RESERVED ;
      tree->ast_node.fencei_reserved.imm = imm;
      tree->ast_node.fencei_reserved.rs = rs;
      tree->ast_node.fencei_reserved.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x07) && ((binary_stream & 0x0000000000007000)>>12 == 0x1)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t imm = (binary_stream & 0x00000000FFF00000)>>20 ;
      tree->ast_node_type = LOAD_FP ;
      tree->ast_node.load_fp.imm = imm;
      tree->ast_node.load_fp.rs1 = rs1;
      tree->ast_node.load_fp.rd = rd;
      tree->ast_node.load_fp.width = HALF;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x07) && ((binary_stream & 0x0000000000007000)>>12 == 0x2)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t imm = (binary_stream & 0x00000000FFF00000)>>20 ;
      tree->ast_node_type = LOAD_FP ;
      tree->ast_node.load_fp.imm = imm;
      tree->ast_node.load_fp.rs1 = rs1;
      tree->ast_node.load_fp.rd = rd;
      tree->ast_node.load_fp.width = WORD;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x07) && ((binary_stream & 0x0000000000007000)>>12 == 0x3)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t imm = (binary_stream & 0x00000000FFF00000)>>20 ;
      tree->ast_node_type = LOAD_FP ;
      tree->ast_node.load_fp.imm = imm;
      tree->ast_node.load_fp.rs1 = rs1;
      tree->ast_node.load_fp.rd = rd;
      tree->ast_node.load_fp.width = DOUBLE;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x27) && ((binary_stream & 0x0000000000007000)>>12 == 0x1)) { 
      uint64_t imm5 = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      uint64_t imm7 = (binary_stream & 0x00000000FE000000)>>25 ;
      tree->ast_node_type = STORE_FP ;
      tree->ast_node.store_fp.imm = (imm7 << 5) | (imm5 << 0);
      tree->ast_node.store_fp.rs2 = rs2;
      tree->ast_node.store_fp.rs1 = rs1;
      tree->ast_node.store_fp.width = HALF;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x27) && ((binary_stream & 0x0000000000007000)>>12 == 0x2)) { 
      uint64_t imm5 = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      uint64_t imm7 = (binary_stream & 0x00000000FE000000)>>25 ;
      tree->ast_node_type = STORE_FP ;
      tree->ast_node.store_fp.imm = (imm7 << 5) | (imm5 << 0);
      tree->ast_node.store_fp.rs2 = rs2;
      tree->ast_node.store_fp.rs1 = rs1;
      tree->ast_node.store_fp.width = WORD;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x27) && ((binary_stream & 0x0000000000007000)>>12 == 0x3)) { 
      uint64_t imm5 = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      uint64_t imm7 = (binary_stream & 0x00000000FE000000)>>25 ;
      tree->ast_node_type = STORE_FP ;
      tree->ast_node.store_fp.imm = (imm7 << 5) | (imm5 << 0);
      tree->ast_node.store_fp.rs2 = rs2;
      tree->ast_node.store_fp.rs1 = rs1;
      tree->ast_node.store_fp.width = DOUBLE;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x43) && ((binary_stream & 0x0000000006000000)>>25 == 0x0)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t rs3 = (binary_stream & 0x00000000F8000000)>>27 ;
          tree->ast_node_type = F_MADD_TYPE_S ;
          tree->ast_node.f_madd_type_s.rs3 = rs3;
          tree->ast_node.f_madd_type_s.rs2 = rs2;
          tree->ast_node.f_madd_type_s.rs1 = rs1;
          tree->ast_node.f_madd_type_s.rm = rm;
          tree->ast_node.f_madd_type_s.rd = rd;
          tree->ast_node.f_madd_type_s.op = FMADD_S;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x47) && ((binary_stream & 0x0000000006000000)>>25 == 0x0)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t rs3 = (binary_stream & 0x00000000F8000000)>>27 ;
          tree->ast_node_type = F_MADD_TYPE_S ;
          tree->ast_node.f_madd_type_s.rs3 = rs3;
          tree->ast_node.f_madd_type_s.rs2 = rs2;
          tree->ast_node.f_madd_type_s.rs1 = rs1;
          tree->ast_node.f_madd_type_s.rm = rm;
          tree->ast_node.f_madd_type_s.rd = rd;
          tree->ast_node.f_madd_type_s.op = FMSUB_S;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x4B) && ((binary_stream & 0x0000000006000000)>>25 == 0x0)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t rs3 = (binary_stream & 0x00000000F8000000)>>27 ;
          tree->ast_node_type = F_MADD_TYPE_S ;
          tree->ast_node.f_madd_type_s.rs3 = rs3;
          tree->ast_node.f_madd_type_s.rs2 = rs2;
          tree->ast_node.f_madd_type_s.rs1 = rs1;
          tree->ast_node.f_madd_type_s.rm = rm;
          tree->ast_node.f_madd_type_s.rd = rd;
          tree->ast_node.f_madd_type_s.op = FNMSUB_S;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x4F) && ((binary_stream & 0x0000000006000000)>>25 == 0x0)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t rs3 = (binary_stream & 0x00000000F8000000)>>27 ;
          tree->ast_node_type = F_MADD_TYPE_S ;
          tree->ast_node.f_madd_type_s.rs3 = rs3;
          tree->ast_node.f_madd_type_s.rs2 = rs2;
          tree->ast_node.f_madd_type_s.rs1 = rs1;
          tree->ast_node.f_madd_type_s.rm = rm;
          tree->ast_node.f_madd_type_s.rd = rd;
          tree->ast_node.f_madd_type_s.op = FNMADD_S;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x00000000FE000000)>>25 == 0x00)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = F_BIN_RM_TYPE_S ;
          tree->ast_node.f_bin_rm_type_s.rs2 = rs2;
          tree->ast_node.f_bin_rm_type_s.rs1 = rs1;
          tree->ast_node.f_bin_rm_type_s.rm = rm;
          tree->ast_node.f_bin_rm_type_s.rd = rd;
          tree->ast_node.f_bin_rm_type_s.op = FADD_S;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x00000000FE000000)>>25 == 0x04)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = F_BIN_RM_TYPE_S ;
          tree->ast_node.f_bin_rm_type_s.rs2 = rs2;
          tree->ast_node.f_bin_rm_type_s.rs1 = rs1;
          tree->ast_node.f_bin_rm_type_s.rm = rm;
          tree->ast_node.f_bin_rm_type_s.rd = rd;
          tree->ast_node.f_bin_rm_type_s.op = FSUB_S;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x00000000FE000000)>>25 == 0x08)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = F_BIN_RM_TYPE_S ;
          tree->ast_node.f_bin_rm_type_s.rs2 = rs2;
          tree->ast_node.f_bin_rm_type_s.rs1 = rs1;
          tree->ast_node.f_bin_rm_type_s.rm = rm;
          tree->ast_node.f_bin_rm_type_s.rd = rd;
          tree->ast_node.f_bin_rm_type_s.op = FMUL_S;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x00000000FE000000)>>25 == 0x0C)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = F_BIN_RM_TYPE_S ;
          tree->ast_node.f_bin_rm_type_s.rs2 = rs2;
          tree->ast_node.f_bin_rm_type_s.rs1 = rs1;
          tree->ast_node.f_bin_rm_type_s.rm = rm;
          tree->ast_node.f_bin_rm_type_s.rd = rd;
          tree->ast_node.f_bin_rm_type_s.op = FDIV_S;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x2C)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_S ;
          tree->ast_node.f_un_rm_type_s.rs1 = rs1;
          tree->ast_node.f_un_rm_type_s.rm = rm;
          tree->ast_node.f_un_rm_type_s.rd = rd;
          tree->ast_node.f_un_rm_type_s.fcvt_s_lu = FSQRT_S;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x60)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_S ;
          tree->ast_node.f_un_rm_type_s.rs1 = rs1;
          tree->ast_node.f_un_rm_type_s.rm = rm;
          tree->ast_node.f_un_rm_type_s.rd = rd;
          tree->ast_node.f_un_rm_type_s.fcvt_s_lu = FCVT_W_S;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x01) && ((binary_stream & 0x00000000FE000000)>>25 == 0x60)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_S ;
          tree->ast_node.f_un_rm_type_s.rs1 = rs1;
          tree->ast_node.f_un_rm_type_s.rm = rm;
          tree->ast_node.f_un_rm_type_s.rd = rd;
          tree->ast_node.f_un_rm_type_s.fcvt_s_lu = FCVT_WU_S;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x68)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_S ;
          tree->ast_node.f_un_rm_type_s.rs1 = rs1;
          tree->ast_node.f_un_rm_type_s.rm = rm;
          tree->ast_node.f_un_rm_type_s.rd = rd;
          tree->ast_node.f_un_rm_type_s.fcvt_s_lu = FCVT_S_W;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x01) && ((binary_stream & 0x00000000FE000000)>>25 == 0x68)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_S ;
          tree->ast_node.f_un_rm_type_s.rs1 = rs1;
          tree->ast_node.f_un_rm_type_s.rm = rm;
          tree->ast_node.f_un_rm_type_s.rd = rd;
          tree->ast_node.f_un_rm_type_s.fcvt_s_lu = FCVT_S_WU;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x02) && ((binary_stream & 0x00000000FE000000)>>25 == 0x60)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_S ;
          tree->ast_node.f_un_rm_type_s.rs1 = rs1;
          tree->ast_node.f_un_rm_type_s.rm = rm;
          tree->ast_node.f_un_rm_type_s.rd = rd;
          tree->ast_node.f_un_rm_type_s.fcvt_s_lu = FCVT_L_S;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x03) && ((binary_stream & 0x00000000FE000000)>>25 == 0x60)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_S ;
          tree->ast_node.f_un_rm_type_s.rs1 = rs1;
          tree->ast_node.f_un_rm_type_s.rm = rm;
          tree->ast_node.f_un_rm_type_s.rd = rd;
          tree->ast_node.f_un_rm_type_s.fcvt_s_lu = FCVT_LU_S;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x02) && ((binary_stream & 0x00000000FE000000)>>25 == 0x68)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_S ;
          tree->ast_node.f_un_rm_type_s.rs1 = rs1;
          tree->ast_node.f_un_rm_type_s.rm = rm;
          tree->ast_node.f_un_rm_type_s.rd = rd;
          tree->ast_node.f_un_rm_type_s.fcvt_s_lu = FCVT_S_L;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x03) && ((binary_stream & 0x00000000FE000000)>>25 == 0x68)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_S ;
          tree->ast_node.f_un_rm_type_s.rs1 = rs1;
          tree->ast_node.f_un_rm_type_s.rm = rm;
          tree->ast_node.f_un_rm_type_s.rd = rd;
          tree->ast_node.f_un_rm_type_s.fcvt_s_lu = FCVT_S_LU;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000FE000000)>>25 == 0x10)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_S ;
      tree->ast_node.f_bin_type_s.rs2 = rs2;
      tree->ast_node.f_bin_type_s.rs1 = rs1;
      tree->ast_node.f_bin_type_s.rd = rd;
      tree->ast_node.f_bin_type_s.fle_s = FSGNJ_S;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FE000000)>>25 == 0x10)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_S ;
      tree->ast_node.f_bin_type_s.rs2 = rs2;
      tree->ast_node.f_bin_type_s.rs1 = rs1;
      tree->ast_node.f_bin_type_s.rd = rd;
      tree->ast_node.f_bin_type_s.fle_s = FSGNJN_S;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000FE000000)>>25 == 0x10)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_S ;
      tree->ast_node.f_bin_type_s.rs2 = rs2;
      tree->ast_node.f_bin_type_s.rs1 = rs1;
      tree->ast_node.f_bin_type_s.rd = rd;
      tree->ast_node.f_bin_type_s.fle_s = FSGNJX_S;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000FE000000)>>25 == 0x14)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_S ;
      tree->ast_node.f_bin_type_s.rs2 = rs2;
      tree->ast_node.f_bin_type_s.rs1 = rs1;
      tree->ast_node.f_bin_type_s.rd = rd;
      tree->ast_node.f_bin_type_s.fle_s = FMIN_S;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FE000000)>>25 == 0x14)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_S ;
      tree->ast_node.f_bin_type_s.rs2 = rs2;
      tree->ast_node.f_bin_type_s.rs1 = rs1;
      tree->ast_node.f_bin_type_s.rd = rd;
      tree->ast_node.f_bin_type_s.fle_s = FMAX_S;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000FE000000)>>25 == 0x50)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_S ;
      tree->ast_node.f_bin_type_s.rs2 = rs2;
      tree->ast_node.f_bin_type_s.rs1 = rs1;
      tree->ast_node.f_bin_type_s.rd = rd;
      tree->ast_node.f_bin_type_s.fle_s = FEQ_S;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FE000000)>>25 == 0x50)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_S ;
      tree->ast_node.f_bin_type_s.rs2 = rs2;
      tree->ast_node.f_bin_type_s.rs1 = rs1;
      tree->ast_node.f_bin_type_s.rd = rd;
      tree->ast_node.f_bin_type_s.fle_s = FLT_S;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000FE000000)>>25 == 0x50)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_S ;
      tree->ast_node.f_bin_type_s.rs2 = rs2;
      tree->ast_node.f_bin_type_s.rs1 = rs1;
      tree->ast_node.f_bin_type_s.rd = rd;
      tree->ast_node.f_bin_type_s.fle_s = FLE_S;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x70)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = F_UN_TYPE_S ;
      tree->ast_node.f_un_type_s.rs1 = rs1;
      tree->ast_node.f_un_type_s.rd = rd;
      tree->ast_node.f_un_type_s.fmv_w_x = FCLASS_S;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x70)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = F_UN_TYPE_S ;
      tree->ast_node.f_un_type_s.rs1 = rs1;
      tree->ast_node.f_un_type_s.rd = rd;
      tree->ast_node.f_un_type_s.fmv_w_x = FMV_X_W;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x78)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = F_UN_TYPE_S ;
      tree->ast_node.f_un_type_s.rs1 = rs1;
      tree->ast_node.f_un_type_s.rd = rd;
      tree->ast_node.f_un_type_s.fmv_w_x = FMV_W_X;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x43) && ((binary_stream & 0x0000000006000000)>>25 == 0x1)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t rs3 = (binary_stream & 0x00000000F8000000)>>27 ;
          tree->ast_node_type = F_MADD_TYPE_D ;
          tree->ast_node.f_madd_type_d.rs3 = rs3;
          tree->ast_node.f_madd_type_d.rs2 = rs2;
          tree->ast_node.f_madd_type_d.rs1 = rs1;
          tree->ast_node.f_madd_type_d.rm = rm;
          tree->ast_node.f_madd_type_d.rd = rd;
          tree->ast_node.f_madd_type_d.op = FMADD_D;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x47) && ((binary_stream & 0x0000000006000000)>>25 == 0x1)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t rs3 = (binary_stream & 0x00000000F8000000)>>27 ;
          tree->ast_node_type = F_MADD_TYPE_D ;
          tree->ast_node.f_madd_type_d.rs3 = rs3;
          tree->ast_node.f_madd_type_d.rs2 = rs2;
          tree->ast_node.f_madd_type_d.rs1 = rs1;
          tree->ast_node.f_madd_type_d.rm = rm;
          tree->ast_node.f_madd_type_d.rd = rd;
          tree->ast_node.f_madd_type_d.op = FMSUB_D;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x4B) && ((binary_stream & 0x0000000006000000)>>25 == 0x1)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t rs3 = (binary_stream & 0x00000000F8000000)>>27 ;
          tree->ast_node_type = F_MADD_TYPE_D ;
          tree->ast_node.f_madd_type_d.rs3 = rs3;
          tree->ast_node.f_madd_type_d.rs2 = rs2;
          tree->ast_node.f_madd_type_d.rs1 = rs1;
          tree->ast_node.f_madd_type_d.rm = rm;
          tree->ast_node.f_madd_type_d.rd = rd;
          tree->ast_node.f_madd_type_d.op = FNMSUB_D;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x4F) && ((binary_stream & 0x0000000006000000)>>25 == 0x1)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t rs3 = (binary_stream & 0x00000000F8000000)>>27 ;
          tree->ast_node_type = F_MADD_TYPE_D ;
          tree->ast_node.f_madd_type_d.rs3 = rs3;
          tree->ast_node.f_madd_type_d.rs2 = rs2;
          tree->ast_node.f_madd_type_d.rs1 = rs1;
          tree->ast_node.f_madd_type_d.rm = rm;
          tree->ast_node.f_madd_type_d.rd = rd;
          tree->ast_node.f_madd_type_d.op = FNMADD_D;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x00000000FE000000)>>25 == 0x01)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = F_BIN_RM_TYPE_D ;
          tree->ast_node.f_bin_rm_type_d.rs2 = rs2;
          tree->ast_node.f_bin_rm_type_d.rs1 = rs1;
          tree->ast_node.f_bin_rm_type_d.rm = rm;
          tree->ast_node.f_bin_rm_type_d.rd = rd;
          tree->ast_node.f_bin_rm_type_d.op = FADD_D;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x00000000FE000000)>>25 == 0x05)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = F_BIN_RM_TYPE_D ;
          tree->ast_node.f_bin_rm_type_d.rs2 = rs2;
          tree->ast_node.f_bin_rm_type_d.rs1 = rs1;
          tree->ast_node.f_bin_rm_type_d.rm = rm;
          tree->ast_node.f_bin_rm_type_d.rd = rd;
          tree->ast_node.f_bin_rm_type_d.op = FSUB_D;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x00000000FE000000)>>25 == 0x09)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = F_BIN_RM_TYPE_D ;
          tree->ast_node.f_bin_rm_type_d.rs2 = rs2;
          tree->ast_node.f_bin_rm_type_d.rs1 = rs1;
          tree->ast_node.f_bin_rm_type_d.rm = rm;
          tree->ast_node.f_bin_rm_type_d.rd = rd;
          tree->ast_node.f_bin_rm_type_d.op = FMUL_D;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x00000000FE000000)>>25 == 0x0D)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = F_BIN_RM_TYPE_D ;
          tree->ast_node.f_bin_rm_type_d.rs2 = rs2;
          tree->ast_node.f_bin_rm_type_d.rs1 = rs1;
          tree->ast_node.f_bin_rm_type_d.rm = rm;
          tree->ast_node.f_bin_rm_type_d.rd = rd;
          tree->ast_node.f_bin_rm_type_d.op = FDIV_D;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x2D)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_D ;
          tree->ast_node.f_un_rm_type_d.rs1 = rs1;
          tree->ast_node.f_un_rm_type_d.rm = rm;
          tree->ast_node.f_un_rm_type_d.rd = rd;
          tree->ast_node.f_un_rm_type_d.fcvt_d_lu = FSQRT_D;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x61)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_D ;
          tree->ast_node.f_un_rm_type_d.rs1 = rs1;
          tree->ast_node.f_un_rm_type_d.rm = rm;
          tree->ast_node.f_un_rm_type_d.rd = rd;
          tree->ast_node.f_un_rm_type_d.fcvt_d_lu = FCVT_W_D;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x01) && ((binary_stream & 0x00000000FE000000)>>25 == 0x61)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_D ;
          tree->ast_node.f_un_rm_type_d.rs1 = rs1;
          tree->ast_node.f_un_rm_type_d.rm = rm;
          tree->ast_node.f_un_rm_type_d.rd = rd;
          tree->ast_node.f_un_rm_type_d.fcvt_d_lu = FCVT_WU_D;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x69)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_D ;
          tree->ast_node.f_un_rm_type_d.rs1 = rs1;
          tree->ast_node.f_un_rm_type_d.rm = rm;
          tree->ast_node.f_un_rm_type_d.rd = rd;
          tree->ast_node.f_un_rm_type_d.fcvt_d_lu = FCVT_D_W;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x01) && ((binary_stream & 0x00000000FE000000)>>25 == 0x69)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_D ;
          tree->ast_node.f_un_rm_type_d.rs1 = rs1;
          tree->ast_node.f_un_rm_type_d.rm = rm;
          tree->ast_node.f_un_rm_type_d.rd = rd;
          tree->ast_node.f_un_rm_type_d.fcvt_d_lu = FCVT_D_WU;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x01) && ((binary_stream & 0x00000000FE000000)>>25 == 0x20)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_D ;
          tree->ast_node.f_un_rm_type_d.rs1 = rs1;
          tree->ast_node.f_un_rm_type_d.rm = rm;
          tree->ast_node.f_un_rm_type_d.rd = rd;
          tree->ast_node.f_un_rm_type_d.fcvt_d_lu = FCVT_S_D;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x21)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_D ;
          tree->ast_node.f_un_rm_type_d.rs1 = rs1;
          tree->ast_node.f_un_rm_type_d.rm = rm;
          tree->ast_node.f_un_rm_type_d.rd = rd;
          tree->ast_node.f_un_rm_type_d.fcvt_d_lu = FCVT_D_S;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x02) && ((binary_stream & 0x00000000FE000000)>>25 == 0x61)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_D ;
          tree->ast_node.f_un_rm_type_d.rs1 = rs1;
          tree->ast_node.f_un_rm_type_d.rm = rm;
          tree->ast_node.f_un_rm_type_d.rd = rd;
          tree->ast_node.f_un_rm_type_d.fcvt_d_lu = FCVT_L_D;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x03) && ((binary_stream & 0x00000000FE000000)>>25 == 0x61)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_D ;
          tree->ast_node.f_un_rm_type_d.rs1 = rs1;
          tree->ast_node.f_un_rm_type_d.rm = rm;
          tree->ast_node.f_un_rm_type_d.rd = rd;
          tree->ast_node.f_un_rm_type_d.fcvt_d_lu = FCVT_LU_D;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x02) && ((binary_stream & 0x00000000FE000000)>>25 == 0x69)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_D ;
          tree->ast_node.f_un_rm_type_d.rs1 = rs1;
          tree->ast_node.f_un_rm_type_d.rm = rm;
          tree->ast_node.f_un_rm_type_d.rd = rd;
          tree->ast_node.f_un_rm_type_d.fcvt_d_lu = FCVT_D_L;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x03) && ((binary_stream & 0x00000000FE000000)>>25 == 0x69)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_D ;
          tree->ast_node.f_un_rm_type_d.rs1 = rs1;
          tree->ast_node.f_un_rm_type_d.rm = rm;
          tree->ast_node.f_un_rm_type_d.rd = rd;
          tree->ast_node.f_un_rm_type_d.fcvt_d_lu = FCVT_D_LU;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000FE000000)>>25 == 0x11)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_D ;
      tree->ast_node.f_bin_type_d.rs2 = rs2;
      tree->ast_node.f_bin_type_d.rs1 = rs1;
      tree->ast_node.f_bin_type_d.rd = rd;
      tree->ast_node.f_bin_type_d.fle_d = FSGNJ_D;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FE000000)>>25 == 0x11)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_D ;
      tree->ast_node.f_bin_type_d.rs2 = rs2;
      tree->ast_node.f_bin_type_d.rs1 = rs1;
      tree->ast_node.f_bin_type_d.rd = rd;
      tree->ast_node.f_bin_type_d.fle_d = FSGNJN_D;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000FE000000)>>25 == 0x11)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_D ;
      tree->ast_node.f_bin_type_d.rs2 = rs2;
      tree->ast_node.f_bin_type_d.rs1 = rs1;
      tree->ast_node.f_bin_type_d.rd = rd;
      tree->ast_node.f_bin_type_d.fle_d = FSGNJX_D;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000FE000000)>>25 == 0x15)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_D ;
      tree->ast_node.f_bin_type_d.rs2 = rs2;
      tree->ast_node.f_bin_type_d.rs1 = rs1;
      tree->ast_node.f_bin_type_d.rd = rd;
      tree->ast_node.f_bin_type_d.fle_d = FMIN_D;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FE000000)>>25 == 0x15)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_D ;
      tree->ast_node.f_bin_type_d.rs2 = rs2;
      tree->ast_node.f_bin_type_d.rs1 = rs1;
      tree->ast_node.f_bin_type_d.rd = rd;
      tree->ast_node.f_bin_type_d.fle_d = FMAX_D;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000FE000000)>>25 == 0x51)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_D ;
      tree->ast_node.f_bin_type_d.rs2 = rs2;
      tree->ast_node.f_bin_type_d.rs1 = rs1;
      tree->ast_node.f_bin_type_d.rd = rd;
      tree->ast_node.f_bin_type_d.fle_d = FEQ_D;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FE000000)>>25 == 0x51)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_D ;
      tree->ast_node.f_bin_type_d.rs2 = rs2;
      tree->ast_node.f_bin_type_d.rs1 = rs1;
      tree->ast_node.f_bin_type_d.rd = rd;
      tree->ast_node.f_bin_type_d.fle_d = FLT_D;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000FE000000)>>25 == 0x51)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_D ;
      tree->ast_node.f_bin_type_d.rs2 = rs2;
      tree->ast_node.f_bin_type_d.rs1 = rs1;
      tree->ast_node.f_bin_type_d.rd = rd;
      tree->ast_node.f_bin_type_d.fle_d = FLE_D;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x71)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = F_UN_TYPE_D ;
      tree->ast_node.f_un_type_d.rs1 = rs1;
      tree->ast_node.f_un_type_d.rd = rd;
      tree->ast_node.f_un_type_d.fmv_d_x = FCLASS_D;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x71)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = F_UN_TYPE_D ;
      tree->ast_node.f_un_type_d.rs1 = rs1;
      tree->ast_node.f_un_type_d.rd = rd;
      tree->ast_node.f_un_type_d.fmv_d_x = FMV_X_D;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x79)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = F_UN_TYPE_D ;
      tree->ast_node.f_un_type_d.rs1 = rs1;
      tree->ast_node.f_un_type_d.rd = rd;
      tree->ast_node.f_un_type_d.fmv_d_x = FMV_D_X;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x73) && ((binary_stream & 0x0000000000000F80)>>7 == 0x00) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000FE000000)>>25 == 0x0B)) { 
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = SINVAL_VMA ;
      tree->ast_node.sinval_vma.rs1 = rs1;
      tree->ast_node.sinval_vma.rs2 = rs2;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x73) && ((binary_stream & 0x0000000000000F80)>>7 == 0x00) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000000F8000)>>15 == 0x00) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x0C)) { 
      tree->ast_node_type = SFENCE_W_INVAL ;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x73) && ((binary_stream & 0x0000000000000F80)>>7 == 0x00) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000000F8000)>>15 == 0x00) && ((binary_stream & 0x0000000001F00000)>>20 == 0x01) && ((binary_stream & 0x00000000FE000000)>>25 == 0x0C)) { 
      tree->ast_node_type = SFENCE_INVAL_IR ;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x1B) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FC000000)>>26 == 0x02)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t shamt = (binary_stream & 0x0000000003F00000)>>20 ;
      tree->ast_node_type = RISCV_SLLIUW ;
      tree->ast_node.riscv_slliuw.shamt = shamt;
      tree->ast_node.riscv_slliuw.rs1 = rs1;
      tree->ast_node.riscv_slliuw.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x3B) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000FE000000)>>25 == 0x04)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBA_RTYPEUW ;
      tree->ast_node.zba_rtypeuw.rs2 = rs2;
      tree->ast_node.zba_rtypeuw.rs1 = rs1;
      tree->ast_node.zba_rtypeuw.rd = rd;
      tree->ast_node.zba_rtypeuw.op = RISCV_ADDUW;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x3B) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000FE000000)>>25 == 0x10)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBA_RTYPEUW ;
      tree->ast_node.zba_rtypeuw.rs2 = rs2;
      tree->ast_node.zba_rtypeuw.rs1 = rs1;
      tree->ast_node.zba_rtypeuw.rd = rd;
      tree->ast_node.zba_rtypeuw.op = RISCV_SH1ADDUW;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x3B) && ((binary_stream & 0x0000000000007000)>>12 == 0x4) && ((binary_stream & 0x00000000FE000000)>>25 == 0x10)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBA_RTYPEUW ;
      tree->ast_node.zba_rtypeuw.rs2 = rs2;
      tree->ast_node.zba_rtypeuw.rs1 = rs1;
      tree->ast_node.zba_rtypeuw.rd = rd;
      tree->ast_node.zba_rtypeuw.op = RISCV_SH2ADDUW;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x3B) && ((binary_stream & 0x0000000000007000)>>12 == 0x6) && ((binary_stream & 0x00000000FE000000)>>25 == 0x10)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBA_RTYPEUW ;
      tree->ast_node.zba_rtypeuw.rs2 = rs2;
      tree->ast_node.zba_rtypeuw.rs1 = rs1;
      tree->ast_node.zba_rtypeuw.rd = rd;
      tree->ast_node.zba_rtypeuw.op = RISCV_SH3ADDUW;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000FE000000)>>25 == 0x10)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBA_RTYPE ;
      tree->ast_node.zba_rtype.rs2 = rs2;
      tree->ast_node.zba_rtype.rs1 = rs1;
      tree->ast_node.zba_rtype.rd = rd;
      tree->ast_node.zba_rtype.op = RISCV_SH1ADD;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x4) && ((binary_stream & 0x00000000FE000000)>>25 == 0x10)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBA_RTYPE ;
      tree->ast_node.zba_rtype.rs2 = rs2;
      tree->ast_node.zba_rtype.rs1 = rs1;
      tree->ast_node.zba_rtype.rd = rd;
      tree->ast_node.zba_rtype.op = RISCV_SH2ADD;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x6) && ((binary_stream & 0x00000000FE000000)>>25 == 0x10)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBA_RTYPE ;
      tree->ast_node.zba_rtype.rs2 = rs2;
      tree->ast_node.zba_rtype.rs1 = rs1;
      tree->ast_node.zba_rtype.rd = rd;
      tree->ast_node.zba_rtype.op = RISCV_SH3ADD;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x1B) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FE000000)>>25 == 0x30)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t shamt = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RISCV_RORIW ;
      tree->ast_node.riscv_roriw.shamt = shamt;
      tree->ast_node.riscv_roriw.rs1 = rs1;
      tree->ast_node.riscv_roriw.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FC000000)>>26 == 0x18)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t shamt = (binary_stream & 0x0000000003F00000)>>20 ;
      tree->ast_node_type = RISCV_RORI ;
      tree->ast_node.riscv_rori.shamt = shamt;
      tree->ast_node.riscv_rori.rs1 = rs1;
      tree->ast_node.riscv_rori.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x3B) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FE000000)>>25 == 0x30)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBB_RTYPEW ;
      tree->ast_node.zbb_rtypew.rs2 = rs2;
      tree->ast_node.zbb_rtypew.rs1 = rs1;
      tree->ast_node.zbb_rtypew.rd = rd;
      tree->ast_node.zbb_rtypew.op = RISCV_ROLW;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x3B) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FE000000)>>25 == 0x30)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBB_RTYPEW ;
      tree->ast_node.zbb_rtypew.rs2 = rs2;
      tree->ast_node.zbb_rtypew.rs1 = rs1;
      tree->ast_node.zbb_rtypew.rd = rd;
      tree->ast_node.zbb_rtypew.op = RISCV_RORW;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x7) && ((binary_stream & 0x00000000FE000000)>>25 == 0x20)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBB_RTYPE ;
      tree->ast_node.zbb_rtype.rs2 = rs2;
      tree->ast_node.zbb_rtype.rs1 = rs1;
      tree->ast_node.zbb_rtype.rd = rd;
      tree->ast_node.zbb_rtype.op = RISCV_ANDN;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x6) && ((binary_stream & 0x00000000FE000000)>>25 == 0x20)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBB_RTYPE ;
      tree->ast_node.zbb_rtype.rs2 = rs2;
      tree->ast_node.zbb_rtype.rs1 = rs1;
      tree->ast_node.zbb_rtype.rd = rd;
      tree->ast_node.zbb_rtype.op = RISCV_ORN;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x4) && ((binary_stream & 0x00000000FE000000)>>25 == 0x20)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBB_RTYPE ;
      tree->ast_node.zbb_rtype.rs2 = rs2;
      tree->ast_node.zbb_rtype.rs1 = rs1;
      tree->ast_node.zbb_rtype.rd = rd;
      tree->ast_node.zbb_rtype.op = RISCV_XNOR;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x6) && ((binary_stream & 0x00000000FE000000)>>25 == 0x05)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBB_RTYPE ;
      tree->ast_node.zbb_rtype.rs2 = rs2;
      tree->ast_node.zbb_rtype.rs1 = rs1;
      tree->ast_node.zbb_rtype.rd = rd;
      tree->ast_node.zbb_rtype.op = RISCV_MAX;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x7) && ((binary_stream & 0x00000000FE000000)>>25 == 0x05)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBB_RTYPE ;
      tree->ast_node.zbb_rtype.rs2 = rs2;
      tree->ast_node.zbb_rtype.rs1 = rs1;
      tree->ast_node.zbb_rtype.rd = rd;
      tree->ast_node.zbb_rtype.op = RISCV_MAXU;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x4) && ((binary_stream & 0x00000000FE000000)>>25 == 0x05)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBB_RTYPE ;
      tree->ast_node.zbb_rtype.rs2 = rs2;
      tree->ast_node.zbb_rtype.rs1 = rs1;
      tree->ast_node.zbb_rtype.rd = rd;
      tree->ast_node.zbb_rtype.op = RISCV_MIN;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FE000000)>>25 == 0x05)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBB_RTYPE ;
      tree->ast_node.zbb_rtype.rs2 = rs2;
      tree->ast_node.zbb_rtype.rs1 = rs1;
      tree->ast_node.zbb_rtype.rd = rd;
      tree->ast_node.zbb_rtype.op = RISCV_MINU;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FE000000)>>25 == 0x30)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBB_RTYPE ;
      tree->ast_node.zbb_rtype.rs2 = rs2;
      tree->ast_node.zbb_rtype.rs1 = rs1;
      tree->ast_node.zbb_rtype.rd = rd;
      tree->ast_node.zbb_rtype.op = RISCV_ROL;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FE000000)>>25 == 0x30)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBB_RTYPE ;
      tree->ast_node.zbb_rtype.rs2 = rs2;
      tree->ast_node.zbb_rtype.rs1 = rs1;
      tree->ast_node.zbb_rtype.rd = rd;
      tree->ast_node.zbb_rtype.op = RISCV_ROR;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x0000000001F00000)>>20 == 0x04) && ((binary_stream & 0x00000000FE000000)>>25 == 0x30)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = ZBB_EXTOP ;
      tree->ast_node.zbb_extop.rs1 = rs1;
      tree->ast_node.zbb_extop.rd = rd;
      tree->ast_node.zbb_extop.op = RISCV_SEXTB;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x0000000001F00000)>>20 == 0x05) && ((binary_stream & 0x00000000FE000000)>>25 == 0x30)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = ZBB_EXTOP ;
      tree->ast_node.zbb_extop.rs1 = rs1;
      tree->ast_node.zbb_extop.rd = rd;
      tree->ast_node.zbb_extop.op = RISCV_SEXTH;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x4) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x04)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = ZBB_EXTOP ;
      tree->ast_node.zbb_extop.rs1 = rs1;
      tree->ast_node.zbb_extop.rd = rd;
      tree->ast_node.zbb_extop.op = RISCV_ZEXTH;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x3B) && ((binary_stream & 0x0000000000007000)>>12 == 0x4) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x04)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = ZBB_EXTOP ;
      tree->ast_node.zbb_extop.rs1 = rs1;
      tree->ast_node.zbb_extop.rd = rd;
      tree->ast_node.zbb_extop.op = RISCV_ZEXTH;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FFF00000)>>20 == 0x698)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = RISCV_REV8 ;
      tree->ast_node.riscv_rev8.rs1 = rs1;
      tree->ast_node.riscv_rev8.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FFF00000)>>20 == 0x6B8)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = RISCV_REV8 ;
      tree->ast_node.riscv_rev8.rs1 = rs1;
      tree->ast_node.riscv_rev8.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FFF00000)>>20 == 0x287)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = RISCV_ORCB ;
      tree->ast_node.riscv_orcb.rs1 = rs1;
      tree->ast_node.riscv_orcb.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FFF00000)>>20 == 0x602)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = RISCV_CPOP ;
      tree->ast_node.riscv_cpop.rs1 = rs1;
      tree->ast_node.riscv_cpop.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x1B) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FFF00000)>>20 == 0x602)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = RISCV_CPOPW ;
      tree->ast_node.riscv_cpopw.rs1 = rs1;
      tree->ast_node.riscv_cpopw.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FFF00000)>>20 == 0x600)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = RISCV_CLZ ;
      tree->ast_node.riscv_clz.rs1 = rs1;
      tree->ast_node.riscv_clz.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x1B) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FFF00000)>>20 == 0x600)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = RISCV_CLZW ;
      tree->ast_node.riscv_clzw.rs1 = rs1;
      tree->ast_node.riscv_clzw.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FFF00000)>>20 == 0x601)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = RISCV_CTZ ;
      tree->ast_node.riscv_ctz.rs1 = rs1;
      tree->ast_node.riscv_ctz.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x1B) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FFF00000)>>20 == 0x601)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = RISCV_CTZW ;
      tree->ast_node.riscv_ctzw.rs1 = rs1;
      tree->ast_node.riscv_ctzw.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FE000000)>>25 == 0x05)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RISCV_CLMUL ;
      tree->ast_node.riscv_clmul.rs2 = rs2;
      tree->ast_node.riscv_clmul.rs1 = rs1;
      tree->ast_node.riscv_clmul.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x3) && ((binary_stream & 0x00000000FE000000)>>25 == 0x05)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RISCV_CLMULH ;
      tree->ast_node.riscv_clmulh.rs2 = rs2;
      tree->ast_node.riscv_clmulh.rs1 = rs1;
      tree->ast_node.riscv_clmulh.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000FE000000)>>25 == 0x05)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RISCV_CLMULR ;
      tree->ast_node.riscv_clmulr.rs2 = rs2;
      tree->ast_node.riscv_clmulr.rs1 = rs1;
      tree->ast_node.riscv_clmulr.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FC000000)>>26 == 0x12)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t shamt = (binary_stream & 0x0000000003F00000)>>20 ;
      tree->ast_node_type = ZBS_IOP ;
      tree->ast_node.zbs_iop.shamt = shamt;
      tree->ast_node.zbs_iop.rs1 = rs1;
      tree->ast_node.zbs_iop.rd = rd;
      tree->ast_node.zbs_iop.op = RISCV_BCLRI;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FC000000)>>26 == 0x12)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t shamt = (binary_stream & 0x0000000003F00000)>>20 ;
      tree->ast_node_type = ZBS_IOP ;
      tree->ast_node.zbs_iop.shamt = shamt;
      tree->ast_node.zbs_iop.rs1 = rs1;
      tree->ast_node.zbs_iop.rd = rd;
      tree->ast_node.zbs_iop.op = RISCV_BEXTI;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FC000000)>>26 == 0x1A)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t shamt = (binary_stream & 0x0000000003F00000)>>20 ;
      tree->ast_node_type = ZBS_IOP ;
      tree->ast_node.zbs_iop.shamt = shamt;
      tree->ast_node.zbs_iop.rs1 = rs1;
      tree->ast_node.zbs_iop.rd = rd;
      tree->ast_node.zbs_iop.op = RISCV_BINVI;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FC000000)>>26 == 0x0A)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t shamt = (binary_stream & 0x0000000003F00000)>>20 ;
      tree->ast_node_type = ZBS_IOP ;
      tree->ast_node.zbs_iop.shamt = shamt;
      tree->ast_node.zbs_iop.rs1 = rs1;
      tree->ast_node.zbs_iop.rd = rd;
      tree->ast_node.zbs_iop.op = RISCV_BSETI;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FE000000)>>25 == 0x24)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBS_RTYPE ;
      tree->ast_node.zbs_rtype.rs2 = rs2;
      tree->ast_node.zbs_rtype.rs1 = rs1;
      tree->ast_node.zbs_rtype.rd = rd;
      tree->ast_node.zbs_rtype.op = RISCV_BCLR;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FE000000)>>25 == 0x24)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBS_RTYPE ;
      tree->ast_node.zbs_rtype.rs2 = rs2;
      tree->ast_node.zbs_rtype.rs1 = rs1;
      tree->ast_node.zbs_rtype.rd = rd;
      tree->ast_node.zbs_rtype.op = RISCV_BEXT;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FE000000)>>25 == 0x34)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBS_RTYPE ;
      tree->ast_node.zbs_rtype.rs2 = rs2;
      tree->ast_node.zbs_rtype.rs1 = rs1;
      tree->ast_node.zbs_rtype.rd = rd;
      tree->ast_node.zbs_rtype.op = RISCV_BINV;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FE000000)>>25 == 0x14)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBS_RTYPE ;
      tree->ast_node.zbs_rtype.rs2 = rs2;
      tree->ast_node.zbs_rtype.rs1 = rs1;
      tree->ast_node.zbs_rtype.rd = rd;
      tree->ast_node.zbs_rtype.op = RISCV_BSET;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x00000000FE000000)>>25 == 0x02)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = F_BIN_RM_TYPE_H ;
          tree->ast_node.f_bin_rm_type_h.rs2 = rs2;
          tree->ast_node.f_bin_rm_type_h.rs1 = rs1;
          tree->ast_node.f_bin_rm_type_h.rm = rm;
          tree->ast_node.f_bin_rm_type_h.rd = rd;
          tree->ast_node.f_bin_rm_type_h.op = FADD_H;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x00000000FE000000)>>25 == 0x06)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = F_BIN_RM_TYPE_H ;
          tree->ast_node.f_bin_rm_type_h.rs2 = rs2;
          tree->ast_node.f_bin_rm_type_h.rs1 = rs1;
          tree->ast_node.f_bin_rm_type_h.rm = rm;
          tree->ast_node.f_bin_rm_type_h.rd = rd;
          tree->ast_node.f_bin_rm_type_h.op = FSUB_H;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x00000000FE000000)>>25 == 0x0A)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = F_BIN_RM_TYPE_H ;
          tree->ast_node.f_bin_rm_type_h.rs2 = rs2;
          tree->ast_node.f_bin_rm_type_h.rs1 = rs1;
          tree->ast_node.f_bin_rm_type_h.rm = rm;
          tree->ast_node.f_bin_rm_type_h.rd = rd;
          tree->ast_node.f_bin_rm_type_h.op = FMUL_H;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x00000000FE000000)>>25 == 0x0E)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = F_BIN_RM_TYPE_H ;
          tree->ast_node.f_bin_rm_type_h.rs2 = rs2;
          tree->ast_node.f_bin_rm_type_h.rs1 = rs1;
          tree->ast_node.f_bin_rm_type_h.rm = rm;
          tree->ast_node.f_bin_rm_type_h.rd = rd;
          tree->ast_node.f_bin_rm_type_h.op = FDIV_H;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x43) && ((binary_stream & 0x0000000006000000)>>25 == 0x2)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t rs3 = (binary_stream & 0x00000000F8000000)>>27 ;
          tree->ast_node_type = F_MADD_TYPE_H ;
          tree->ast_node.f_madd_type_h.rs3 = rs3;
          tree->ast_node.f_madd_type_h.rs2 = rs2;
          tree->ast_node.f_madd_type_h.rs1 = rs1;
          tree->ast_node.f_madd_type_h.rm = rm;
          tree->ast_node.f_madd_type_h.rd = rd;
          tree->ast_node.f_madd_type_h.op = FMADD_H;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x47) && ((binary_stream & 0x0000000006000000)>>25 == 0x2)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t rs3 = (binary_stream & 0x00000000F8000000)>>27 ;
          tree->ast_node_type = F_MADD_TYPE_H ;
          tree->ast_node.f_madd_type_h.rs3 = rs3;
          tree->ast_node.f_madd_type_h.rs2 = rs2;
          tree->ast_node.f_madd_type_h.rs1 = rs1;
          tree->ast_node.f_madd_type_h.rm = rm;
          tree->ast_node.f_madd_type_h.rd = rd;
          tree->ast_node.f_madd_type_h.op = FMSUB_H;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x4B) && ((binary_stream & 0x0000000006000000)>>25 == 0x2)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t rs3 = (binary_stream & 0x00000000F8000000)>>27 ;
          tree->ast_node_type = F_MADD_TYPE_H ;
          tree->ast_node.f_madd_type_h.rs3 = rs3;
          tree->ast_node.f_madd_type_h.rs2 = rs2;
          tree->ast_node.f_madd_type_h.rs1 = rs1;
          tree->ast_node.f_madd_type_h.rm = rm;
          tree->ast_node.f_madd_type_h.rd = rd;
          tree->ast_node.f_madd_type_h.op = FNMSUB_H;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x4F) && ((binary_stream & 0x0000000006000000)>>25 == 0x2)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t rs3 = (binary_stream & 0x00000000F8000000)>>27 ;
          tree->ast_node_type = F_MADD_TYPE_H ;
          tree->ast_node.f_madd_type_h.rs3 = rs3;
          tree->ast_node.f_madd_type_h.rs2 = rs2;
          tree->ast_node.f_madd_type_h.rs1 = rs1;
          tree->ast_node.f_madd_type_h.rm = rm;
          tree->ast_node.f_madd_type_h.rd = rd;
          tree->ast_node.f_madd_type_h.op = FNMADD_H;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000FE000000)>>25 == 0x12)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_H ;
      tree->ast_node.f_bin_type_h.rs2 = rs2;
      tree->ast_node.f_bin_type_h.rs1 = rs1;
      tree->ast_node.f_bin_type_h.rd = rd;
      tree->ast_node.f_bin_type_h.fle_h = FSGNJ_H;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FE000000)>>25 == 0x12)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_H ;
      tree->ast_node.f_bin_type_h.rs2 = rs2;
      tree->ast_node.f_bin_type_h.rs1 = rs1;
      tree->ast_node.f_bin_type_h.rd = rd;
      tree->ast_node.f_bin_type_h.fle_h = FSGNJN_H;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000FE000000)>>25 == 0x12)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_H ;
      tree->ast_node.f_bin_type_h.rs2 = rs2;
      tree->ast_node.f_bin_type_h.rs1 = rs1;
      tree->ast_node.f_bin_type_h.rd = rd;
      tree->ast_node.f_bin_type_h.fle_h = FSGNJX_H;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000FE000000)>>25 == 0x16)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_H ;
      tree->ast_node.f_bin_type_h.rs2 = rs2;
      tree->ast_node.f_bin_type_h.rs1 = rs1;
      tree->ast_node.f_bin_type_h.rd = rd;
      tree->ast_node.f_bin_type_h.fle_h = FMIN_H;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FE000000)>>25 == 0x16)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_H ;
      tree->ast_node.f_bin_type_h.rs2 = rs2;
      tree->ast_node.f_bin_type_h.rs1 = rs1;
      tree->ast_node.f_bin_type_h.rd = rd;
      tree->ast_node.f_bin_type_h.fle_h = FMAX_H;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000FE000000)>>25 == 0x52)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_H ;
      tree->ast_node.f_bin_type_h.rs2 = rs2;
      tree->ast_node.f_bin_type_h.rs1 = rs1;
      tree->ast_node.f_bin_type_h.rd = rd;
      tree->ast_node.f_bin_type_h.fle_h = FEQ_H;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FE000000)>>25 == 0x52)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_H ;
      tree->ast_node.f_bin_type_h.rs2 = rs2;
      tree->ast_node.f_bin_type_h.rs1 = rs1;
      tree->ast_node.f_bin_type_h.rd = rd;
      tree->ast_node.f_bin_type_h.fle_h = FLT_H;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000FE000000)>>25 == 0x52)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = F_BIN_TYPE_H ;
      tree->ast_node.f_bin_type_h.rs2 = rs2;
      tree->ast_node.f_bin_type_h.rs1 = rs1;
      tree->ast_node.f_bin_type_h.rd = rd;
      tree->ast_node.f_bin_type_h.fle_h = FLE_H;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x2E)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_H ;
          tree->ast_node.f_un_rm_type_h.rs1 = rs1;
          tree->ast_node.f_un_rm_type_h.rm = rm;
          tree->ast_node.f_un_rm_type_h.rd = rd;
          tree->ast_node.f_un_rm_type_h.fcvt_h_lu = FSQRT_H;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x62)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_H ;
          tree->ast_node.f_un_rm_type_h.rs1 = rs1;
          tree->ast_node.f_un_rm_type_h.rm = rm;
          tree->ast_node.f_un_rm_type_h.rd = rd;
          tree->ast_node.f_un_rm_type_h.fcvt_h_lu = FCVT_W_H;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x01) && ((binary_stream & 0x00000000FE000000)>>25 == 0x62)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_H ;
          tree->ast_node.f_un_rm_type_h.rs1 = rs1;
          tree->ast_node.f_un_rm_type_h.rm = rm;
          tree->ast_node.f_un_rm_type_h.rd = rd;
          tree->ast_node.f_un_rm_type_h.fcvt_h_lu = FCVT_WU_H;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x6A)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_H ;
          tree->ast_node.f_un_rm_type_h.rs1 = rs1;
          tree->ast_node.f_un_rm_type_h.rm = rm;
          tree->ast_node.f_un_rm_type_h.rd = rd;
          tree->ast_node.f_un_rm_type_h.fcvt_h_lu = FCVT_H_W;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x01) && ((binary_stream & 0x00000000FE000000)>>25 == 0x6A)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_H ;
          tree->ast_node.f_un_rm_type_h.rs1 = rs1;
          tree->ast_node.f_un_rm_type_h.rm = rm;
          tree->ast_node.f_un_rm_type_h.rd = rd;
          tree->ast_node.f_un_rm_type_h.fcvt_h_lu = FCVT_H_WU;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x22)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_H ;
          tree->ast_node.f_un_rm_type_h.rs1 = rs1;
          tree->ast_node.f_un_rm_type_h.rm = rm;
          tree->ast_node.f_un_rm_type_h.rd = rd;
          tree->ast_node.f_un_rm_type_h.fcvt_h_lu = FCVT_H_S;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x01) && ((binary_stream & 0x00000000FE000000)>>25 == 0x22)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_H ;
          tree->ast_node.f_un_rm_type_h.rs1 = rs1;
          tree->ast_node.f_un_rm_type_h.rm = rm;
          tree->ast_node.f_un_rm_type_h.rd = rd;
          tree->ast_node.f_un_rm_type_h.fcvt_h_lu = FCVT_H_D;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x02) && ((binary_stream & 0x00000000FE000000)>>25 == 0x20)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_H ;
          tree->ast_node.f_un_rm_type_h.rs1 = rs1;
          tree->ast_node.f_un_rm_type_h.rm = rm;
          tree->ast_node.f_un_rm_type_h.rd = rd;
          tree->ast_node.f_un_rm_type_h.fcvt_h_lu = FCVT_S_H;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x02) && ((binary_stream & 0x00000000FE000000)>>25 == 0x21)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_H ;
          tree->ast_node.f_un_rm_type_h.rs1 = rs1;
          tree->ast_node.f_un_rm_type_h.rm = rm;
          tree->ast_node.f_un_rm_type_h.rd = rd;
          tree->ast_node.f_un_rm_type_h.fcvt_h_lu = FCVT_D_H;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x02) && ((binary_stream & 0x00000000FE000000)>>25 == 0x62)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_H ;
          tree->ast_node.f_un_rm_type_h.rs1 = rs1;
          tree->ast_node.f_un_rm_type_h.rm = rm;
          tree->ast_node.f_un_rm_type_h.rd = rd;
          tree->ast_node.f_un_rm_type_h.fcvt_h_lu = FCVT_L_H;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x03) && ((binary_stream & 0x00000000FE000000)>>25 == 0x62)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_H ;
          tree->ast_node.f_un_rm_type_h.rs1 = rs1;
          tree->ast_node.f_un_rm_type_h.rm = rm;
          tree->ast_node.f_un_rm_type_h.rd = rd;
          tree->ast_node.f_un_rm_type_h.fcvt_h_lu = FCVT_LU_H;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x02) && ((binary_stream & 0x00000000FE000000)>>25 == 0x6A)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_H ;
          tree->ast_node.f_un_rm_type_h.rs1 = rs1;
          tree->ast_node.f_un_rm_type_h.rm = rm;
          tree->ast_node.f_un_rm_type_h.rd = rd;
          tree->ast_node.f_un_rm_type_h.fcvt_h_lu = FCVT_H_L;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x03) && ((binary_stream & 0x00000000FE000000)>>25 == 0x6A)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = F_UN_RM_TYPE_H ;
          tree->ast_node.f_un_rm_type_h.rs1 = rs1;
          tree->ast_node.f_un_rm_type_h.rm = rm;
          tree->ast_node.f_un_rm_type_h.rd = rd;
          tree->ast_node.f_un_rm_type_h.fcvt_h_lu = FCVT_H_LU;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x72)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = F_UN_TYPE_H ;
      tree->ast_node.f_un_type_h.rs1 = rs1;
      tree->ast_node.f_un_type_h.rd = rd;
      tree->ast_node.f_un_type_h.fmv_h_x = FCLASS_H;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x72)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = F_UN_TYPE_H ;
      tree->ast_node.f_un_type_h.rs1 = rs1;
      tree->ast_node.f_un_type_h.rd = rd;
      tree->ast_node.f_un_type_h.fmv_h_x = FMV_X_H;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FE000000)>>25 == 0x7A)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = F_UN_TYPE_H ;
      tree->ast_node.f_un_type_h.rs1 = rs1;
      tree->ast_node.f_un_type_h.rd = rd;
      tree->ast_node.f_un_type_h.fmv_h_x = FMV_H_X;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x0000000001F00000)>>20 == 0x01) && ((binary_stream & 0x00000000FE000000)>>25 == 0x7A)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = RISCV_FLI_H ;
      tree->ast_node.riscv_fli_h.constantidx = rs1;
      tree->ast_node.riscv_fli_h.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x0000000001F00000)>>20 == 0x01) && ((binary_stream & 0x00000000FE000000)>>25 == 0x78)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = RISCV_FLI_S ;
      tree->ast_node.riscv_fli_s.constantidx = rs1;
      tree->ast_node.riscv_fli_s.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x0000000001F00000)>>20 == 0x01) && ((binary_stream & 0x00000000FE000000)>>25 == 0x79)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = RISCV_FLI_D ;
      tree->ast_node.riscv_fli_d.constantidx = rs1;
      tree->ast_node.riscv_fli_d.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000FE000000)>>25 == 0x16)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RISCV_FMINM_H ;
      tree->ast_node.riscv_fminm_h.rs2 = rs2;
      tree->ast_node.riscv_fminm_h.rs1 = rs1;
      tree->ast_node.riscv_fminm_h.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x3) && ((binary_stream & 0x00000000FE000000)>>25 == 0x16)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RISCV_FMAXM_H ;
      tree->ast_node.riscv_fmaxm_h.rs2 = rs2;
      tree->ast_node.riscv_fmaxm_h.rs1 = rs1;
      tree->ast_node.riscv_fmaxm_h.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000FE000000)>>25 == 0x14)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RISCV_FMINM_S ;
      tree->ast_node.riscv_fminm_s.rs2 = rs2;
      tree->ast_node.riscv_fminm_s.rs1 = rs1;
      tree->ast_node.riscv_fminm_s.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x3) && ((binary_stream & 0x00000000FE000000)>>25 == 0x14)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RISCV_FMAXM_S ;
      tree->ast_node.riscv_fmaxm_s.rs2 = rs2;
      tree->ast_node.riscv_fmaxm_s.rs1 = rs1;
      tree->ast_node.riscv_fmaxm_s.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000FE000000)>>25 == 0x15)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RISCV_FMINM_D ;
      tree->ast_node.riscv_fminm_d.rs2 = rs2;
      tree->ast_node.riscv_fminm_d.rs1 = rs1;
      tree->ast_node.riscv_fminm_d.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x3) && ((binary_stream & 0x00000000FE000000)>>25 == 0x15)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RISCV_FMAXM_D ;
      tree->ast_node.riscv_fmaxm_d.rs2 = rs2;
      tree->ast_node.riscv_fmaxm_d.rs1 = rs1;
      tree->ast_node.riscv_fmaxm_d.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x04) && ((binary_stream & 0x00000000FE000000)>>25 == 0x22)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = RISCV_FROUND_H ;
          tree->ast_node.riscv_fround_h.rs1 = rs1;
          tree->ast_node.riscv_fround_h.rm = rm;
          tree->ast_node.riscv_fround_h.rd = rd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x05) && ((binary_stream & 0x00000000FE000000)>>25 == 0x22)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = RISCV_FROUNDNX_H ;
          tree->ast_node.riscv_froundnx_h.rs1 = rs1;
          tree->ast_node.riscv_froundnx_h.rm = rm;
          tree->ast_node.riscv_froundnx_h.rd = rd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x04) && ((binary_stream & 0x00000000FE000000)>>25 == 0x20)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = RISCV_FROUND_S ;
          tree->ast_node.riscv_fround_s.rs1 = rs1;
          tree->ast_node.riscv_fround_s.rm = rm;
          tree->ast_node.riscv_fround_s.rd = rd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x05) && ((binary_stream & 0x00000000FE000000)>>25 == 0x20)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = RISCV_FROUNDNX_S ;
          tree->ast_node.riscv_froundnx_s.rs1 = rs1;
          tree->ast_node.riscv_froundnx_s.rm = rm;
          tree->ast_node.riscv_froundnx_s.rd = rd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x04) && ((binary_stream & 0x00000000FE000000)>>25 == 0x21)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = RISCV_FROUND_D ;
          tree->ast_node.riscv_fround_d.rs1 = rs1;
          tree->ast_node.riscv_fround_d.rm = rm;
          tree->ast_node.riscv_fround_d.rd = rd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000001F00000)>>20 == 0x05) && ((binary_stream & 0x00000000FE000000)>>25 == 0x21)) { 
      uint64_t rm = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                rm = RM_DYN ;
        break; 
        case 0x1: 
                rm = RM_RTZ ;
        break; 
        case 0x3: 
                rm = RM_RUP ;
        break; 
        case 0x2: 
                rm = RM_RDN ;
        break; 
        case 0x4: 
                rm = RM_RMM ;
        break; 
        case 0x0: 
                rm = RM_RNE ;
        break; 
 
      }
      if (rm != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = RISCV_FROUNDNX_D ;
          tree->ast_node.riscv_froundnx_d.rs1 = rs1;
          tree->ast_node.riscv_froundnx_d.rm = rm;
          tree->ast_node.riscv_froundnx_d.rd = rd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x0000000001F00000)>>20 == 0x01) && ((binary_stream & 0x00000000FE000000)>>25 == 0x71)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = RISCV_FMVH_X_D ;
      tree->ast_node.riscv_fmvh_x_d.rs1 = rs1;
      tree->ast_node.riscv_fmvh_x_d.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x00000000FE000000)>>25 == 0x59)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RISCV_FMVP_D_X ;
      tree->ast_node.riscv_fmvp_d_x.rs2 = rs2;
      tree->ast_node.riscv_fmvp_d_x.rs1 = rs1;
      tree->ast_node.riscv_fmvp_d_x.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x4) && ((binary_stream & 0x00000000FE000000)>>25 == 0x52)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RISCV_FLEQ_H ;
      tree->ast_node.riscv_fleq_h.rs2 = rs2;
      tree->ast_node.riscv_fleq_h.rs1 = rs1;
      tree->ast_node.riscv_fleq_h.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FE000000)>>25 == 0x52)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RISCV_FLTQ_H ;
      tree->ast_node.riscv_fltq_h.rs2 = rs2;
      tree->ast_node.riscv_fltq_h.rs1 = rs1;
      tree->ast_node.riscv_fltq_h.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x4) && ((binary_stream & 0x00000000FE000000)>>25 == 0x50)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RISCV_FLEQ_S ;
      tree->ast_node.riscv_fleq_s.rs2 = rs2;
      tree->ast_node.riscv_fleq_s.rs1 = rs1;
      tree->ast_node.riscv_fleq_s.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FE000000)>>25 == 0x50)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RISCV_FLTQ_S ;
      tree->ast_node.riscv_fltq_s.rs2 = rs2;
      tree->ast_node.riscv_fltq_s.rs1 = rs1;
      tree->ast_node.riscv_fltq_s.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x4) && ((binary_stream & 0x00000000FE000000)>>25 == 0x51)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RISCV_FLEQ_D ;
      tree->ast_node.riscv_fleq_d.rs2 = rs2;
      tree->ast_node.riscv_fleq_d.rs1 = rs1;
      tree->ast_node.riscv_fleq_d.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FE000000)>>25 == 0x51)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RISCV_FLTQ_D ;
      tree->ast_node.riscv_fltq_d.rs2 = rs2;
      tree->ast_node.riscv_fltq_d.rs1 = rs1;
      tree->ast_node.riscv_fltq_d.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x53) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x0000000001F00000)>>20 == 0x08) && ((binary_stream & 0x00000000FE000000)>>25 == 0x61)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = RISCV_FCVTMOD_W_D ;
      tree->ast_node.riscv_fcvtmod_w_d.rs1 = rs1;
      tree->ast_node.riscv_fcvtmod_w_d.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x000000003E000000)>>25 == 0x08) && ((binary_stream & 0x00000000C0000000)>>30 == 0x0)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = SHA256SUM0 ;
      tree->ast_node.sha256sum0.rs1 = rs1;
      tree->ast_node.sha256sum0.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x0000000001F00000)>>20 == 0x01) && ((binary_stream & 0x000000003E000000)>>25 == 0x08) && ((binary_stream & 0x00000000C0000000)>>30 == 0x0)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = SHA256SUM1 ;
      tree->ast_node.sha256sum1.rs1 = rs1;
      tree->ast_node.sha256sum1.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x0000000001F00000)>>20 == 0x02) && ((binary_stream & 0x000000003E000000)>>25 == 0x08) && ((binary_stream & 0x00000000C0000000)>>30 == 0x0)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = SHA256SIG0 ;
      tree->ast_node.sha256sig0.rs1 = rs1;
      tree->ast_node.sha256sig0.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x0000000001F00000)>>20 == 0x03) && ((binary_stream & 0x000000003E000000)>>25 == 0x08) && ((binary_stream & 0x00000000C0000000)>>30 == 0x0)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = SHA256SIG1 ;
      tree->ast_node.sha256sig1.rs1 = rs1;
      tree->ast_node.sha256sig1.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x000000003E000000)>>25 == 0x13)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      uint64_t bs = (binary_stream & 0x00000000C0000000)>>30 ;
      tree->ast_node_type = AES32ESMI ;
      tree->ast_node.aes32esmi.bs = bs;
      tree->ast_node.aes32esmi.rs2 = rs2;
      tree->ast_node.aes32esmi.rs1 = rs1;
      tree->ast_node.aes32esmi.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x000000003E000000)>>25 == 0x11)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      uint64_t bs = (binary_stream & 0x00000000C0000000)>>30 ;
      tree->ast_node_type = AES32ESI ;
      tree->ast_node.aes32esi.bs = bs;
      tree->ast_node.aes32esi.rs2 = rs2;
      tree->ast_node.aes32esi.rs1 = rs1;
      tree->ast_node.aes32esi.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x000000003E000000)>>25 == 0x17)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      uint64_t bs = (binary_stream & 0x00000000C0000000)>>30 ;
      tree->ast_node_type = AES32DSMI ;
      tree->ast_node.aes32dsmi.bs = bs;
      tree->ast_node.aes32dsmi.rs2 = rs2;
      tree->ast_node.aes32dsmi.rs1 = rs1;
      tree->ast_node.aes32dsmi.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x000000003E000000)>>25 == 0x15)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      uint64_t bs = (binary_stream & 0x00000000C0000000)>>30 ;
      tree->ast_node_type = AES32DSI ;
      tree->ast_node.aes32dsi.bs = bs;
      tree->ast_node.aes32dsi.rs2 = rs2;
      tree->ast_node.aes32dsi.rs1 = rs1;
      tree->ast_node.aes32dsi.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x000000003E000000)>>25 == 0x08) && ((binary_stream & 0x00000000C0000000)>>30 == 0x1)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = SHA512SUM0R ;
      tree->ast_node.sha512sum0r.rs2 = rs2;
      tree->ast_node.sha512sum0r.rs1 = rs1;
      tree->ast_node.sha512sum0r.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x000000003E000000)>>25 == 0x09) && ((binary_stream & 0x00000000C0000000)>>30 == 0x1)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = SHA512SUM1R ;
      tree->ast_node.sha512sum1r.rs2 = rs2;
      tree->ast_node.sha512sum1r.rs1 = rs1;
      tree->ast_node.sha512sum1r.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x000000003E000000)>>25 == 0x0A) && ((binary_stream & 0x00000000C0000000)>>30 == 0x1)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = SHA512SIG0L ;
      tree->ast_node.sha512sig0l.rs2 = rs2;
      tree->ast_node.sha512sig0l.rs1 = rs1;
      tree->ast_node.sha512sig0l.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x000000003E000000)>>25 == 0x0E) && ((binary_stream & 0x00000000C0000000)>>30 == 0x1)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = SHA512SIG0H ;
      tree->ast_node.sha512sig0h.rs2 = rs2;
      tree->ast_node.sha512sig0h.rs1 = rs1;
      tree->ast_node.sha512sig0h.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x000000003E000000)>>25 == 0x0B) && ((binary_stream & 0x00000000C0000000)>>30 == 0x1)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = SHA512SIG1L ;
      tree->ast_node.sha512sig1l.rs2 = rs2;
      tree->ast_node.sha512sig1l.rs1 = rs1;
      tree->ast_node.sha512sig1l.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x000000003E000000)>>25 == 0x0F) && ((binary_stream & 0x00000000C0000000)>>30 == 0x1)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = SHA512SIG1H ;
      tree->ast_node.sha512sig1h.rs2 = rs2;
      tree->ast_node.sha512sig1h.rs1 = rs1;
      tree->ast_node.sha512sig1h.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x0000000001000000)>>24 == 0x1) && ((binary_stream & 0x000000003E000000)>>25 == 0x18) && ((binary_stream & 0x00000000C0000000)>>30 == 0x0)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rnum = (binary_stream & 0x0000000000F00000)>>20 ;
      tree->ast_node_type = AES64KS1I ;
      tree->ast_node.aes64ks1i.rnum = rnum;
      tree->ast_node.aes64ks1i.rs1 = rs1;
      tree->ast_node.aes64ks1i.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x000000003E000000)>>25 == 0x18) && ((binary_stream & 0x00000000C0000000)>>30 == 0x0)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = AES64IM ;
      tree->ast_node.aes64im.rs1 = rs1;
      tree->ast_node.aes64im.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x000000003E000000)>>25 == 0x1F) && ((binary_stream & 0x00000000C0000000)>>30 == 0x1)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = AES64KS2 ;
      tree->ast_node.aes64ks2.rs2 = rs2;
      tree->ast_node.aes64ks2.rs1 = rs1;
      tree->ast_node.aes64ks2.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x000000003E000000)>>25 == 0x1B) && ((binary_stream & 0x00000000C0000000)>>30 == 0x0)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = AES64ESM ;
      tree->ast_node.aes64esm.rs2 = rs2;
      tree->ast_node.aes64esm.rs1 = rs1;
      tree->ast_node.aes64esm.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x000000003E000000)>>25 == 0x19) && ((binary_stream & 0x00000000C0000000)>>30 == 0x0)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = AES64ES ;
      tree->ast_node.aes64es.rs2 = rs2;
      tree->ast_node.aes64es.rs1 = rs1;
      tree->ast_node.aes64es.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x000000003E000000)>>25 == 0x1F) && ((binary_stream & 0x00000000C0000000)>>30 == 0x0)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = AES64DSM ;
      tree->ast_node.aes64dsm.rs2 = rs2;
      tree->ast_node.aes64dsm.rs1 = rs1;
      tree->ast_node.aes64dsm.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x000000003E000000)>>25 == 0x1D) && ((binary_stream & 0x00000000C0000000)>>30 == 0x0)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = AES64DS ;
      tree->ast_node.aes64ds.rs2 = rs2;
      tree->ast_node.aes64ds.rs1 = rs1;
      tree->ast_node.aes64ds.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x0000000001F00000)>>20 == 0x04) && ((binary_stream & 0x000000003E000000)>>25 == 0x08) && ((binary_stream & 0x00000000C0000000)>>30 == 0x0)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = SHA512SUM0 ;
      tree->ast_node.sha512sum0.rs1 = rs1;
      tree->ast_node.sha512sum0.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x0000000001F00000)>>20 == 0x05) && ((binary_stream & 0x000000003E000000)>>25 == 0x08) && ((binary_stream & 0x00000000C0000000)>>30 == 0x0)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = SHA512SUM1 ;
      tree->ast_node.sha512sum1.rs1 = rs1;
      tree->ast_node.sha512sum1.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x0000000001F00000)>>20 == 0x06) && ((binary_stream & 0x000000003E000000)>>25 == 0x08) && ((binary_stream & 0x00000000C0000000)>>30 == 0x0)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = SHA512SIG0 ;
      tree->ast_node.sha512sig0.rs1 = rs1;
      tree->ast_node.sha512sig0.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x0000000001F00000)>>20 == 0x07) && ((binary_stream & 0x000000003E000000)>>25 == 0x08) && ((binary_stream & 0x00000000C0000000)>>30 == 0x0)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = SHA512SIG1 ;
      tree->ast_node.sha512sig1.rs1 = rs1;
      tree->ast_node.sha512sig1.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x0000000001F00000)>>20 == 0x08) && ((binary_stream & 0x000000003E000000)>>25 == 0x08) && ((binary_stream & 0x00000000C0000000)>>30 == 0x0)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = SM3P0 ;
      tree->ast_node.sm3p0.rs1 = rs1;
      tree->ast_node.sm3p0.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x0000000001F00000)>>20 == 0x09) && ((binary_stream & 0x000000003E000000)>>25 == 0x08) && ((binary_stream & 0x00000000C0000000)>>30 == 0x0)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = SM3P1 ;
      tree->ast_node.sm3p1.rs1 = rs1;
      tree->ast_node.sm3p1.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x000000003E000000)>>25 == 0x18)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      uint64_t bs = (binary_stream & 0x00000000C0000000)>>30 ;
      tree->ast_node_type = SM4ED ;
      tree->ast_node.sm4ed.bs = bs;
      tree->ast_node.sm4ed.rs2 = rs2;
      tree->ast_node.sm4ed.rs1 = rs1;
      tree->ast_node.sm4ed.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x000000003E000000)>>25 == 0x1A)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      uint64_t bs = (binary_stream & 0x00000000C0000000)>>30 ;
      tree->ast_node_type = SM4KS ;
      tree->ast_node.sm4ks.bs = bs;
      tree->ast_node.sm4ks.rs2 = rs2;
      tree->ast_node.sm4ks.rs1 = rs1;
      tree->ast_node.sm4ks.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x4) && ((binary_stream & 0x00000000FE000000)>>25 == 0x04)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBKB_RTYPE ;
      tree->ast_node.zbkb_rtype.rs2 = rs2;
      tree->ast_node.zbkb_rtype.rs1 = rs1;
      tree->ast_node.zbkb_rtype.rd = rd;
      tree->ast_node.zbkb_rtype.op = RISCV_PACK;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x7) && ((binary_stream & 0x00000000FE000000)>>25 == 0x04)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBKB_RTYPE ;
      tree->ast_node.zbkb_rtype.rs2 = rs2;
      tree->ast_node.zbkb_rtype.rs1 = rs1;
      tree->ast_node.zbkb_rtype.rd = rd;
      tree->ast_node.zbkb_rtype.op = RISCV_PACKH;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x3B) && ((binary_stream & 0x0000000000007000)>>12 == 0x4) && ((binary_stream & 0x00000000FE000000)>>25 == 0x04)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZBKB_PACKW ;
      tree->ast_node.zbkb_packw.rs2 = rs2;
      tree->ast_node.zbkb_packw.rs1 = rs1;
      tree->ast_node.zbkb_packw.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FFF00000)>>20 == 0x08F)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = RISCV_ZIP ;
      tree->ast_node.riscv_zip.rs1 = rs1;
      tree->ast_node.riscv_zip.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FFF00000)>>20 == 0x08F)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = RISCV_UNZIP ;
      tree->ast_node.riscv_unzip.rs1 = rs1;
      tree->ast_node.riscv_unzip.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x13) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FFF00000)>>20 == 0x687)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = RISCV_BREV8 ;
      tree->ast_node.riscv_brev8.rs1 = rs1;
      tree->ast_node.riscv_brev8.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x4) && ((binary_stream & 0x00000000FE000000)>>25 == 0x14)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RISCV_XPERM8 ;
      tree->ast_node.riscv_xperm8.rs2 = rs2;
      tree->ast_node.riscv_xperm8.rs1 = rs1;
      tree->ast_node.riscv_xperm8.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000FE000000)>>25 == 0x14)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = RISCV_XPERM4 ;
      tree->ast_node.riscv_xperm4.rs2 = rs2;
      tree->ast_node.riscv_xperm4.rs1 = rs1;
      tree->ast_node.riscv_xperm4.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x00000000FE000000)>>25 == 0x07)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZICOND_RTYPE ;
      tree->ast_node.zicond_rtype.rs2 = rs2;
      tree->ast_node.zicond_rtype.rs1 = rs1;
      tree->ast_node.zicond_rtype.rd = rd;
      tree->ast_node.zicond_rtype.riscv_czero_nez = RISCV_CZERO_EQZ;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x33) && ((binary_stream & 0x0000000000007000)>>12 == 0x7) && ((binary_stream & 0x00000000FE000000)>>25 == 0x07)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = ZICOND_RTYPE ;
      tree->ast_node.zicond_rtype.rs2 = rs2;
      tree->ast_node.zicond_rtype.rs1 = rs1;
      tree->ast_node.zicond_rtype.rd = rd;
      tree->ast_node.zicond_rtype.riscv_czero_nez = RISCV_CZERO_NEZ;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x7) && ((binary_stream & 0x00000000F0000000)>>28 == 0x0)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t lmul = (binary_stream & 0x0000000000700000)>>20 ;
      uint64_t sew = (binary_stream & 0x0000000003800000)>>23 ;
      uint64_t ta = (binary_stream & 0x0000000004000000)>>26 ;
      uint64_t ma = (binary_stream & 0x0000000008000000)>>27 ;
      tree->ast_node_type = VSETVLI ;
      tree->ast_node.vsetvli.ma = ma;
      tree->ast_node.vsetvli.ta = ta;
      tree->ast_node.vsetvli.sew = sew;
      tree->ast_node.vsetvli.lmul = lmul;
      tree->ast_node.vsetvli.rs1 = rs1;
      tree->ast_node.vsetvli.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x7) && ((binary_stream & 0x00000000FE000000)>>25 == 0x40)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = VSETVL ;
      tree->ast_node.vsetvl.rs2 = rs2;
      tree->ast_node.vsetvl.rs1 = rs1;
      tree->ast_node.vsetvl.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x7) && ((binary_stream & 0x00000000F0000000)>>28 == 0xC)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t uimm = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t lmul = (binary_stream & 0x0000000000700000)>>20 ;
      uint64_t sew = (binary_stream & 0x0000000003800000)>>23 ;
      uint64_t ta = (binary_stream & 0x0000000004000000)>>26 ;
      uint64_t ma = (binary_stream & 0x0000000008000000)>>27 ;
      tree->ast_node_type = VSETIVLI ;
      tree->ast_node.vsetivli.ma = ma;
      tree->ast_node.vsetivli.ta = ta;
      tree->ast_node.vsetivli.sew = sew;
      tree->ast_node.vsetivli.lmul = lmul;
      tree->ast_node.vsetivli.uimm = uimm;
      tree->ast_node.vsetivli.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x0)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x06: 
                funct6 = VV_VMAXU ;
        break; 
        case 0x25: 
                funct6 = VV_VSLL ;
        break; 
        case 0x23: 
                funct6 = VV_VSSUB ;
        break; 
        case 0x09: 
                funct6 = VV_VAND ;
        break; 
        case 0x00: 
                funct6 = VV_VADD ;
        break; 
        case 0x0A: 
                funct6 = VV_VOR ;
        break; 
        case 0x0C: 
                funct6 = VV_VRGATHER ;
        break; 
        case 0x2B: 
                funct6 = VV_VSSRA ;
        break; 
        case 0x02: 
                funct6 = VV_VSUB ;
        break; 
        case 0x20: 
                funct6 = VV_VSADDU ;
        break; 
        case 0x28: 
                funct6 = VV_VSRL ;
        break; 
        case 0x0B: 
                funct6 = VV_VXOR ;
        break; 
        case 0x07: 
                funct6 = VV_VMAX ;
        break; 
        case 0x04: 
                funct6 = VV_VMINU ;
        break; 
        case 0x22: 
                funct6 = VV_VSSUBU ;
        break; 
        case 0x27: 
                funct6 = VV_VSMUL ;
        break; 
        case 0x0E: 
                funct6 = VV_VRGATHEREI16 ;
        break; 
        case 0x2A: 
                funct6 = VV_VSSRL ;
        break; 
        case 0x05: 
                funct6 = VV_VMIN ;
        break; 
        case 0x21: 
                funct6 = VV_VSADD ;
        break; 
        case 0x29: 
                funct6 = VV_VSRA ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = VVTYPE ;
          tree->ast_node.vvtype.funct6 = funct6;
          tree->ast_node.vvtype.vm = vm;
          tree->ast_node.vvtype.vs2 = vs2;
          tree->ast_node.vvtype.vs1 = vs1;
          tree->ast_node.vvtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x0)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x2C: 
                funct6 = NVS_VNSRL ;
        break; 
        case 0x2D: 
                funct6 = NVS_VNSRA ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = NVSTYPE ;
          tree->ast_node.nvstype.funct6 = funct6;
          tree->ast_node.nvstype.vm = vm;
          tree->ast_node.nvstype.vs2 = vs2;
          tree->ast_node.nvstype.vs1 = vs1;
          tree->ast_node.nvstype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x0)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x2E: 
                funct6 = NV_VNCLIPU ;
        break; 
        case 0x2F: 
                funct6 = NV_VNCLIP ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = NVTYPE ;
          tree->ast_node.nvtype.funct6 = funct6;
          tree->ast_node.nvtype.vm = vm;
          tree->ast_node.nvtype.vs2 = vs2;
          tree->ast_node.nvtype.vs1 = vs1;
          tree->ast_node.nvtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x0000000002000000)>>25 == 0x0) && ((binary_stream & 0x00000000FC000000)>>26 == 0x17)) { 
      uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = MASKTYPEV ;
      tree->ast_node.masktypev.vs2 = vs2;
      tree->ast_node.masktypev.vs1 = vs1;
      tree->ast_node.masktypev.vd = vd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x0000000002000000)>>25 == 0x1) && ((binary_stream & 0x00000000FC000000)>>26 == 0x17)) { 
      uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = MOVETYPEV ;
      tree->ast_node.movetypev.vs1 = vs1;
      tree->ast_node.movetypev.vd = vd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x4)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x06: 
                funct6 = VX_VMAXU ;
        break; 
        case 0x25: 
                funct6 = VX_VSLL ;
        break; 
        case 0x23: 
                funct6 = VX_VSSUB ;
        break; 
        case 0x09: 
                funct6 = VX_VAND ;
        break; 
        case 0x00: 
                funct6 = VX_VADD ;
        break; 
        case 0x0A: 
                funct6 = VX_VOR ;
        break; 
        case 0x2B: 
                funct6 = VX_VSSRA ;
        break; 
        case 0x02: 
                funct6 = VX_VSUB ;
        break; 
        case 0x20: 
                funct6 = VX_VSADDU ;
        break; 
        case 0x28: 
                funct6 = VX_VSRL ;
        break; 
        case 0x0B: 
                funct6 = VX_VXOR ;
        break; 
        case 0x07: 
                funct6 = VX_VMAX ;
        break; 
        case 0x04: 
                funct6 = VX_VMINU ;
        break; 
        case 0x22: 
                funct6 = VX_VSSUBU ;
        break; 
        case 0x27: 
                funct6 = VX_VSMUL ;
        break; 
        case 0x2A: 
                funct6 = VX_VSSRL ;
        break; 
        case 0x03: 
                funct6 = VX_VRSUB ;
        break; 
        case 0x05: 
                funct6 = VX_VMIN ;
        break; 
        case 0x21: 
                funct6 = VX_VSADD ;
        break; 
        case 0x29: 
                funct6 = VX_VSRA ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = VXTYPE ;
          tree->ast_node.vxtype.funct6 = funct6;
          tree->ast_node.vxtype.vm = vm;
          tree->ast_node.vxtype.vs2 = vs2;
          tree->ast_node.vxtype.rs1 = rs1;
          tree->ast_node.vxtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x4)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x2C: 
                funct6 = NXS_VNSRL ;
        break; 
        case 0x2D: 
                funct6 = NXS_VNSRA ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = NXSTYPE ;
          tree->ast_node.nxstype.funct6 = funct6;
          tree->ast_node.nxstype.vm = vm;
          tree->ast_node.nxstype.vs2 = vs2;
          tree->ast_node.nxstype.rs1 = rs1;
          tree->ast_node.nxstype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x4)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x2E: 
                funct6 = NX_VNCLIPU ;
        break; 
        case 0x2F: 
                funct6 = NX_VNCLIP ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = NXTYPE ;
          tree->ast_node.nxtype.funct6 = funct6;
          tree->ast_node.nxtype.vm = vm;
          tree->ast_node.nxtype.vs2 = vs2;
          tree->ast_node.nxtype.rs1 = rs1;
          tree->ast_node.nxtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x4)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x0E: 
                funct6 = VX_VSLIDEUP ;
        break; 
        case 0x0C: 
                funct6 = VX_VRGATHER ;
        break; 
        case 0x0F: 
                funct6 = VX_VSLIDEDOWN ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = VXSG ;
          tree->ast_node.vxsg.funct6 = funct6;
          tree->ast_node.vxsg.vm = vm;
          tree->ast_node.vxsg.vs2 = vs2;
          tree->ast_node.vxsg.rs1 = rs1;
          tree->ast_node.vxsg.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x4) && ((binary_stream & 0x0000000002000000)>>25 == 0x0) && ((binary_stream & 0x00000000FC000000)>>26 == 0x17)) { 
      uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = MASKTYPEX ;
      tree->ast_node.masktypex.vs2 = vs2;
      tree->ast_node.masktypex.rs1 = rs1;
      tree->ast_node.masktypex.vd = vd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x4) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x0000000002000000)>>25 == 0x1) && ((binary_stream & 0x00000000FC000000)>>26 == 0x17)) { 
      uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = MOVETYPEX ;
      tree->ast_node.movetypex.rs1 = rs1;
      tree->ast_node.movetypex.vd = vd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x3)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x28: 
                funct6 = VI_VSRL ;
        break; 
        case 0x0B: 
                funct6 = VI_VXOR ;
        break; 
        case 0x25: 
                funct6 = VI_VSLL ;
        break; 
        case 0x09: 
                funct6 = VI_VAND ;
        break; 
        case 0x00: 
                funct6 = VI_VADD ;
        break; 
        case 0x0A: 
                funct6 = VI_VOR ;
        break; 
        case 0x2A: 
                funct6 = VI_VSSRL ;
        break; 
        case 0x03: 
                funct6 = VI_VRSUB ;
        break; 
        case 0x21: 
                funct6 = VI_VSADD ;
        break; 
        case 0x2B: 
                funct6 = VI_VSSRA ;
        break; 
        case 0x29: 
                funct6 = VI_VSRA ;
        break; 
        case 0x20: 
                funct6 = VI_VSADDU ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t simm = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = VITYPE ;
          tree->ast_node.vitype.funct6 = funct6;
          tree->ast_node.vitype.vm = vm;
          tree->ast_node.vitype.vs2 = vs2;
          tree->ast_node.vitype.simm = simm;
          tree->ast_node.vitype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x3)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x2C: 
                funct6 = NIS_VNSRL ;
        break; 
        case 0x2D: 
                funct6 = NIS_VNSRA ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t simm = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = NISTYPE ;
          tree->ast_node.nistype.funct6 = funct6;
          tree->ast_node.nistype.vm = vm;
          tree->ast_node.nistype.vs2 = vs2;
          tree->ast_node.nistype.simm = simm;
          tree->ast_node.nistype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x3)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x2E: 
                funct6 = NI_VNCLIPU ;
        break; 
        case 0x2F: 
                funct6 = NI_VNCLIP ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t simm = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = NITYPE ;
          tree->ast_node.nitype.funct6 = funct6;
          tree->ast_node.nitype.vm = vm;
          tree->ast_node.nitype.vs2 = vs2;
          tree->ast_node.nitype.simm = simm;
          tree->ast_node.nitype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x3)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x0E: 
                funct6 = VI_VSLIDEUP ;
        break; 
        case 0x0C: 
                funct6 = VI_VRGATHER ;
        break; 
        case 0x0F: 
                funct6 = VI_VSLIDEDOWN ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t simm = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = VISG ;
          tree->ast_node.visg.funct6 = funct6;
          tree->ast_node.visg.vm = vm;
          tree->ast_node.visg.vs2 = vs2;
          tree->ast_node.visg.simm = simm;
          tree->ast_node.visg.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x3) && ((binary_stream & 0x0000000002000000)>>25 == 0x0) && ((binary_stream & 0x00000000FC000000)>>26 == 0x17)) { 
      uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t simm = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = MASKTYPEI ;
      tree->ast_node.masktypei.vs2 = vs2;
      tree->ast_node.masktypei.simm = simm;
      tree->ast_node.masktypei.vd = vd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x3) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x0000000002000000)>>25 == 0x1) && ((binary_stream & 0x00000000FC000000)>>26 == 0x17)) { 
      uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t simm = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = MOVETYPEI ;
      tree->ast_node.movetypei.vd = vd;
      tree->ast_node.movetypei.simm = simm;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x3) && ((binary_stream & 0x0000000002000000)>>25 == 0x1) && ((binary_stream & 0x00000000FC000000)>>26 == 0x27)) { 
      uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t simm = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = VMVRTYPE ;
      tree->ast_node.vmvrtype.vs2 = vs2;
      tree->ast_node.vmvrtype.simm = simm;
      tree->ast_node.vmvrtype.vd = vd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x2)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x08: 
                funct6 = MVV_VAADDU ;
        break; 
        case 0x0B: 
                funct6 = MVV_VASUB ;
        break; 
        case 0x25: 
                funct6 = MVV_VMUL ;
        break; 
        case 0x23: 
                funct6 = MVV_VREM ;
        break; 
        case 0x09: 
                funct6 = MVV_VAADD ;
        break; 
        case 0x27: 
                funct6 = MVV_VMULH ;
        break; 
        case 0x22: 
                funct6 = MVV_VREMU ;
        break; 
        case 0x0A: 
                funct6 = MVV_VASUBU ;
        break; 
        case 0x24: 
                funct6 = MVV_VMULHU ;
        break; 
        case 0x26: 
                funct6 = MVV_VMULHSU ;
        break; 
        case 0x21: 
                funct6 = MVV_VDIV ;
        break; 
        case 0x20: 
                funct6 = MVV_VDIVU ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = MVVTYPE ;
          tree->ast_node.mvvtype.funct6 = funct6;
          tree->ast_node.mvvtype.vm = vm;
          tree->ast_node.mvvtype.vs2 = vs2;
          tree->ast_node.mvvtype.vs1 = vs1;
          tree->ast_node.mvvtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x2)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x2D: 
                funct6 = MVV_VMACC ;
        break; 
        case 0x2F: 
                funct6 = MVV_VNMSAC ;
        break; 
        case 0x2B: 
                funct6 = MVV_VNMSUB ;
        break; 
        case 0x29: 
                funct6 = MVV_VMADD ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = MVVMATYPE ;
          tree->ast_node.mvvmatype.funct6 = funct6;
          tree->ast_node.mvvmatype.vm = vm;
          tree->ast_node.mvvmatype.vs2 = vs2;
          tree->ast_node.mvvmatype.vs1 = vs1;
          tree->ast_node.mvvmatype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x2)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x3B: 
                funct6 = WVV_VWMUL ;
        break; 
        case 0x33: 
                funct6 = WVV_VSUB ;
        break; 
        case 0x3A: 
                funct6 = WVV_VWMULSU ;
        break; 
        case 0x31: 
                funct6 = WVV_VADD ;
        break; 
        case 0x30: 
                funct6 = WVV_VADDU ;
        break; 
        case 0x32: 
                funct6 = WVV_VSUBU ;
        break; 
        case 0x38: 
                funct6 = WVV_VWMULU ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = WVVTYPE ;
          tree->ast_node.wvvtype.funct6 = funct6;
          tree->ast_node.wvvtype.vm = vm;
          tree->ast_node.wvvtype.vs2 = vs2;
          tree->ast_node.wvvtype.vs1 = vs1;
          tree->ast_node.wvvtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x2)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x36: 
                funct6 = WV_VSUBU ;
        break; 
        case 0x35: 
                funct6 = WV_VADD ;
        break; 
        case 0x37: 
                funct6 = WV_VSUB ;
        break; 
        case 0x34: 
                funct6 = WV_VADDU ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = WVTYPE ;
          tree->ast_node.wvtype.funct6 = funct6;
          tree->ast_node.wvtype.vm = vm;
          tree->ast_node.wvtype.vs2 = vs2;
          tree->ast_node.wvtype.vs1 = vs1;
          tree->ast_node.wvtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x2)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x3D: 
                funct6 = WMVV_VWMACC ;
        break; 
        case 0x3C: 
                funct6 = WMVV_VWMACCU ;
        break; 
        case 0x3F: 
                funct6 = WMVV_VWMACCSU ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = WMVVTYPE ;
          tree->ast_node.wmvvtype.funct6 = funct6;
          tree->ast_node.wmvvtype.vm = vm;
          tree->ast_node.wmvvtype.vs2 = vs2;
          tree->ast_node.wmvvtype.vs1 = vs1;
          tree->ast_node.wmvvtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000FC000000)>>26 == 0x12)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000000F8000)>>15) {
        case 0x06: 
                funct6 = VEXT2_ZVF2 ;
        break; 
        case 0x07: 
                funct6 = VEXT2_SVF2 ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = VEXT2TYPE ;
          tree->ast_node.vext2type.funct6 = funct6;
          tree->ast_node.vext2type.vm = vm;
          tree->ast_node.vext2type.vs2 = vs2;
          tree->ast_node.vext2type.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000FC000000)>>26 == 0x12)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000000F8000)>>15) {
        case 0x04: 
                funct6 = VEXT4_ZVF4 ;
        break; 
        case 0x05: 
                funct6 = VEXT4_SVF4 ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = VEXT4TYPE ;
          tree->ast_node.vext4type.funct6 = funct6;
          tree->ast_node.vext4type.vm = vm;
          tree->ast_node.vext4type.vs2 = vs2;
          tree->ast_node.vext4type.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000FC000000)>>26 == 0x12)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000000F8000)>>15) {
        case 0x03: 
                funct6 = VEXT8_SVF8 ;
        break; 
        case 0x02: 
                funct6 = VEXT8_ZVF8 ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = VEXT8TYPE ;
          tree->ast_node.vext8type.funct6 = funct6;
          tree->ast_node.vext8type.vm = vm;
          tree->ast_node.vext8type.vs2 = vs2;
          tree->ast_node.vext8type.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000000F8000)>>15 == 0x00) && ((binary_stream & 0x0000000002000000)>>25 == 0x1) && ((binary_stream & 0x00000000FC000000)>>26 == 0x10)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = VMVXS ;
      tree->ast_node.vmvxs.vs2 = vs2;
      tree->ast_node.vmvxs.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x0000000002000000)>>25 == 0x1) && ((binary_stream & 0x00000000FC000000)>>26 == 0x17)) { 
      uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = MVVCOMPRESS ;
      tree->ast_node.mvvcompress.vs2 = vs2;
      tree->ast_node.mvvcompress.vs1 = vs1;
      tree->ast_node.mvvcompress.vd = vd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x6)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x08: 
                funct6 = MVX_VAADDU ;
        break; 
        case 0x0B: 
                funct6 = MVX_VASUB ;
        break; 
        case 0x25: 
                funct6 = MVX_VMUL ;
        break; 
        case 0x23: 
                funct6 = MVX_VREM ;
        break; 
        case 0x09: 
                funct6 = MVX_VAADD ;
        break; 
        case 0x27: 
                funct6 = MVX_VMULH ;
        break; 
        case 0x22: 
                funct6 = MVX_VREMU ;
        break; 
        case 0x0A: 
                funct6 = MVX_VASUBU ;
        break; 
        case 0x24: 
                funct6 = MVX_VMULHU ;
        break; 
        case 0x0E: 
                funct6 = MVX_VSLIDE1UP ;
        break; 
        case 0x26: 
                funct6 = MVX_VMULHSU ;
        break; 
        case 0x21: 
                funct6 = MVX_VDIV ;
        break; 
        case 0x0F: 
                funct6 = MVX_VSLIDE1DOWN ;
        break; 
        case 0x20: 
                funct6 = MVX_VDIVU ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = MVXTYPE ;
          tree->ast_node.mvxtype.funct6 = funct6;
          tree->ast_node.mvxtype.vm = vm;
          tree->ast_node.mvxtype.vs2 = vs2;
          tree->ast_node.mvxtype.rs1 = rs1;
          tree->ast_node.mvxtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x6)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x2D: 
                funct6 = MVX_VMACC ;
        break; 
        case 0x2F: 
                funct6 = MVX_VNMSAC ;
        break; 
        case 0x2B: 
                funct6 = MVX_VNMSUB ;
        break; 
        case 0x29: 
                funct6 = MVX_VMADD ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = MVXMATYPE ;
          tree->ast_node.mvxmatype.funct6 = funct6;
          tree->ast_node.mvxmatype.vm = vm;
          tree->ast_node.mvxmatype.vs2 = vs2;
          tree->ast_node.mvxmatype.rs1 = rs1;
          tree->ast_node.mvxmatype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x6)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x3B: 
                funct6 = WVX_VWMUL ;
        break; 
        case 0x33: 
                funct6 = WVX_VSUB ;
        break; 
        case 0x3A: 
                funct6 = WVX_VWMULSU ;
        break; 
        case 0x31: 
                funct6 = WVX_VADD ;
        break; 
        case 0x30: 
                funct6 = WVX_VADDU ;
        break; 
        case 0x32: 
                funct6 = WVX_VSUBU ;
        break; 
        case 0x38: 
                funct6 = WVX_VWMULU ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = WVXTYPE ;
          tree->ast_node.wvxtype.funct6 = funct6;
          tree->ast_node.wvxtype.vm = vm;
          tree->ast_node.wvxtype.vs2 = vs2;
          tree->ast_node.wvxtype.rs1 = rs1;
          tree->ast_node.wvxtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x6)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x36: 
                funct6 = WX_VSUBU ;
        break; 
        case 0x35: 
                funct6 = WX_VADD ;
        break; 
        case 0x37: 
                funct6 = WX_VSUB ;
        break; 
        case 0x34: 
                funct6 = WX_VADDU ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = WXTYPE ;
          tree->ast_node.wxtype.funct6 = funct6;
          tree->ast_node.wxtype.vm = vm;
          tree->ast_node.wxtype.vs2 = vs2;
          tree->ast_node.wxtype.rs1 = rs1;
          tree->ast_node.wxtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x6)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x3E: 
                funct6 = WMVX_VWMACCUS ;
        break; 
        case 0x3D: 
                funct6 = WMVX_VWMACC ;
        break; 
        case 0x3C: 
                funct6 = WMVX_VWMACCU ;
        break; 
        case 0x3F: 
                funct6 = WMVX_VWMACCSU ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = WMVXTYPE ;
          tree->ast_node.wmvxtype.funct6 = funct6;
          tree->ast_node.wmvxtype.vm = vm;
          tree->ast_node.wmvxtype.vs2 = vs2;
          tree->ast_node.wmvxtype.rs1 = rs1;
          tree->ast_node.wmvxtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x6) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x0000000002000000)>>25 == 0x1) && ((binary_stream & 0x00000000FC000000)>>26 == 0x10)) { 
      uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = VMVSX ;
      tree->ast_node.vmvsx.rs1 = rs1;
      tree->ast_node.vmvsx.vd = vd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x1)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x08: 
                funct6 = FVV_VSGNJ ;
        break; 
        case 0x06: 
                funct6 = FVV_VMAX ;
        break; 
        case 0x04: 
                funct6 = FVV_VMIN ;
        break; 
        case 0x09: 
                funct6 = FVV_VSGNJN ;
        break; 
        case 0x00: 
                funct6 = FVV_VADD ;
        break; 
        case 0x0A: 
                funct6 = FVV_VSGNJX ;
        break; 
        case 0x24: 
                funct6 = FVV_VMUL ;
        break; 
        case 0x02: 
                funct6 = FVV_VSUB ;
        break; 
        case 0x20: 
                funct6 = FVV_VDIV ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = FVVTYPE ;
          tree->ast_node.fvvtype.funct6 = funct6;
          tree->ast_node.fvvtype.vm = vm;
          tree->ast_node.fvvtype.vs2 = vs2;
          tree->ast_node.fvvtype.vs1 = vs1;
          tree->ast_node.fvvtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x1)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x28: 
                funct6 = FVV_VMADD ;
        break; 
        case 0x2C: 
                funct6 = FVV_VMACC ;
        break; 
        case 0x2D: 
                funct6 = FVV_VNMACC ;
        break; 
        case 0x2E: 
                funct6 = FVV_VMSAC ;
        break; 
        case 0x2F: 
                funct6 = FVV_VNMSAC ;
        break; 
        case 0x2A: 
                funct6 = FVV_VMSUB ;
        break; 
        case 0x2B: 
                funct6 = FVV_VNMSUB ;
        break; 
        case 0x29: 
                funct6 = FVV_VNMADD ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = FVVMATYPE ;
          tree->ast_node.fvvmatype.funct6 = funct6;
          tree->ast_node.fvvmatype.vm = vm;
          tree->ast_node.fvvmatype.vs2 = vs2;
          tree->ast_node.fvvmatype.vs1 = vs1;
          tree->ast_node.fvvmatype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x1)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x30: 
                funct6 = FWVV_VADD ;
        break; 
        case 0x32: 
                funct6 = FWVV_VSUB ;
        break; 
        case 0x38: 
                funct6 = FWVV_VMUL ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = FWVVTYPE ;
          tree->ast_node.fwvvtype.funct6 = funct6;
          tree->ast_node.fwvvtype.vm = vm;
          tree->ast_node.fwvvtype.vs2 = vs2;
          tree->ast_node.fwvvtype.vs1 = vs1;
          tree->ast_node.fwvvtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x1)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x3E: 
                funct6 = FWVV_VMSAC ;
        break; 
        case 0x3D: 
                funct6 = FWVV_VNMACC ;
        break; 
        case 0x3C: 
                funct6 = FWVV_VMACC ;
        break; 
        case 0x3F: 
                funct6 = FWVV_VNMSAC ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs2 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs1 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = FWVVMATYPE ;
          tree->ast_node.fwvvmatype.funct6 = funct6;
          tree->ast_node.fwvvmatype.vm = vm;
          tree->ast_node.fwvvmatype.vs1 = vs1;
          tree->ast_node.fwvvmatype.vs2 = vs2;
          tree->ast_node.fwvvmatype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x1)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x36: 
                funct6 = FWV_VSUB ;
        break; 
        case 0x34: 
                funct6 = FWV_VADD ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = FWVTYPE ;
          tree->ast_node.fwvtype.funct6 = funct6;
          tree->ast_node.fwvtype.vm = vm;
          tree->ast_node.fwvtype.vs2 = vs2;
          tree->ast_node.fwvtype.vs1 = vs1;
          tree->ast_node.fwvtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FC000000)>>26 == 0x12)) { 
      uint64_t vfunary0 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000000F8000)>>15) {
        case 0x01: 
                vfunary0 = FV_CVT_X_F ;
        break; 
        case 0x06: 
                vfunary0 = FV_CVT_RTZ_XU_F ;
        break; 
        case 0x07: 
                vfunary0 = FV_CVT_RTZ_X_F ;
        break; 
        case 0x00: 
                vfunary0 = FV_CVT_XU_F ;
        break; 
        case 0x03: 
                vfunary0 = FV_CVT_F_X ;
        break; 
        case 0x02: 
                vfunary0 = FV_CVT_F_XU ;
        break; 
 
      }
      if (vfunary0 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = VFUNARY0 ;
          tree->ast_node.vfunary0.vm = vm;
          tree->ast_node.vfunary0.vs2 = vs2;
          tree->ast_node.vfunary0.vfunary0 = vfunary0;
          tree->ast_node.vfunary0.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FC000000)>>26 == 0x12)) { 
      uint64_t vfwunary0 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000000F8000)>>15) {
        case 0x08: 
                vfwunary0 = FWV_CVT_XU_F ;
        break; 
        case 0x0B: 
                vfwunary0 = FWV_CVT_F_X ;
        break; 
        case 0x09: 
                vfwunary0 = FWV_CVT_X_F ;
        break; 
        case 0x0A: 
                vfwunary0 = FWV_CVT_F_XU ;
        break; 
        case 0x0E: 
                vfwunary0 = FWV_CVT_RTZ_XU_F ;
        break; 
        case 0x0C: 
                vfwunary0 = FWV_CVT_F_F ;
        break; 
        case 0x0F: 
                vfwunary0 = FWV_CVT_RTZ_X_F ;
        break; 
 
      }
      if (vfwunary0 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = VFWUNARY0 ;
          tree->ast_node.vfwunary0.vm = vm;
          tree->ast_node.vfwunary0.vs2 = vs2;
          tree->ast_node.vfwunary0.vfwunary0 = vfwunary0;
          tree->ast_node.vfwunary0.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FC000000)>>26 == 0x12)) { 
      uint64_t vfnunary0 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000000F8000)>>15) {
        case 0x15: 
                vfnunary0 = FNV_CVT_ROD_F_F ;
        break; 
        case 0x13: 
                vfnunary0 = FNV_CVT_F_X ;
        break; 
        case 0x10: 
                vfnunary0 = FNV_CVT_XU_F ;
        break; 
        case 0x12: 
                vfnunary0 = FNV_CVT_F_XU ;
        break; 
        case 0x14: 
                vfnunary0 = FNV_CVT_F_F ;
        break; 
        case 0x16: 
                vfnunary0 = FNV_CVT_RTZ_XU_F ;
        break; 
        case 0x11: 
                vfnunary0 = FNV_CVT_X_F ;
        break; 
        case 0x17: 
                vfnunary0 = FNV_CVT_RTZ_X_F ;
        break; 
 
      }
      if (vfnunary0 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = VFNUNARY0 ;
          tree->ast_node.vfnunary0.vm = vm;
          tree->ast_node.vfnunary0.vs2 = vs2;
          tree->ast_node.vfnunary0.vfnunary0 = vfnunary0;
          tree->ast_node.vfnunary0.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000FC000000)>>26 == 0x13)) { 
      uint64_t vfunary1 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000000F8000)>>15) {
        case 0x04: 
                vfunary1 = FVV_VRSQRT7 ;
        break; 
        case 0x10: 
                vfunary1 = FVV_VCLASS ;
        break; 
        case 0x00: 
                vfunary1 = FVV_VSQRT ;
        break; 
        case 0x05: 
                vfunary1 = FVV_VREC7 ;
        break; 
 
      }
      if (vfunary1 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = VFUNARY1 ;
          tree->ast_node.vfunary1.vm = vm;
          tree->ast_node.vfunary1.vs2 = vs2;
          tree->ast_node.vfunary1.vfunary1 = vfunary1;
          tree->ast_node.vfunary1.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x1) && ((binary_stream & 0x00000000000F8000)>>15 == 0x00) && ((binary_stream & 0x0000000002000000)>>25 == 0x1) && ((binary_stream & 0x00000000FC000000)>>26 == 0x10)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = VFMVFS ;
      tree->ast_node.vfmvfs.vs2 = vs2;
      tree->ast_node.vfmvfs.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x5)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x08: 
                funct6 = VF_VSGNJ ;
        break; 
        case 0x06: 
                funct6 = VF_VMAX ;
        break; 
        case 0x04: 
                funct6 = VF_VMIN ;
        break; 
        case 0x09: 
                funct6 = VF_VSGNJN ;
        break; 
        case 0x27: 
                funct6 = VF_VRSUB ;
        break; 
        case 0x00: 
                funct6 = VF_VADD ;
        break; 
        case 0x0A: 
                funct6 = VF_VSGNJX ;
        break; 
        case 0x24: 
                funct6 = VF_VMUL ;
        break; 
        case 0x0E: 
                funct6 = VF_VSLIDE1UP ;
        break; 
        case 0x21: 
                funct6 = VF_VRDIV ;
        break; 
        case 0x02: 
                funct6 = VF_VSUB ;
        break; 
        case 0x0F: 
                funct6 = VF_VSLIDE1DOWN ;
        break; 
        case 0x20: 
                funct6 = VF_VDIV ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = FVFTYPE ;
          tree->ast_node.fvftype.funct6 = funct6;
          tree->ast_node.fvftype.vm = vm;
          tree->ast_node.fvftype.vs2 = vs2;
          tree->ast_node.fvftype.rs1 = rs1;
          tree->ast_node.fvftype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x5)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x28: 
                funct6 = VF_VMADD ;
        break; 
        case 0x2C: 
                funct6 = VF_VMACC ;
        break; 
        case 0x2D: 
                funct6 = VF_VNMACC ;
        break; 
        case 0x2E: 
                funct6 = VF_VMSAC ;
        break; 
        case 0x2F: 
                funct6 = VF_VNMSAC ;
        break; 
        case 0x2A: 
                funct6 = VF_VMSUB ;
        break; 
        case 0x2B: 
                funct6 = VF_VNMSUB ;
        break; 
        case 0x29: 
                funct6 = VF_VNMADD ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = FVFMATYPE ;
          tree->ast_node.fvfmatype.funct6 = funct6;
          tree->ast_node.fvfmatype.vm = vm;
          tree->ast_node.fvfmatype.vs2 = vs2;
          tree->ast_node.fvfmatype.rs1 = rs1;
          tree->ast_node.fvfmatype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x5)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x30: 
                funct6 = FWVF_VADD ;
        break; 
        case 0x32: 
                funct6 = FWVF_VSUB ;
        break; 
        case 0x38: 
                funct6 = FWVF_VMUL ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = FWVFTYPE ;
          tree->ast_node.fwvftype.funct6 = funct6;
          tree->ast_node.fwvftype.vm = vm;
          tree->ast_node.fwvftype.vs2 = vs2;
          tree->ast_node.fwvftype.rs1 = rs1;
          tree->ast_node.fwvftype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x5)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x3E: 
                funct6 = FWVF_VMSAC ;
        break; 
        case 0x3D: 
                funct6 = FWVF_VNMACC ;
        break; 
        case 0x3C: 
                funct6 = FWVF_VMACC ;
        break; 
        case 0x3F: 
                funct6 = FWVF_VNMSAC ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = FWVFMATYPE ;
          tree->ast_node.fwvfmatype.funct6 = funct6;
          tree->ast_node.fwvfmatype.vm = vm;
          tree->ast_node.fwvfmatype.rs1 = rs1;
          tree->ast_node.fwvfmatype.vs2 = vs2;
          tree->ast_node.fwvfmatype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x5)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x36: 
                funct6 = FWF_VSUB ;
        break; 
        case 0x34: 
                funct6 = FWF_VADD ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = FWFTYPE ;
          tree->ast_node.fwftype.funct6 = funct6;
          tree->ast_node.fwftype.vm = vm;
          tree->ast_node.fwftype.vs2 = vs2;
          tree->ast_node.fwftype.rs1 = rs1;
          tree->ast_node.fwftype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x0000000002000000)>>25 == 0x0) && ((binary_stream & 0x00000000FC000000)>>26 == 0x17)) { 
      uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      tree->ast_node_type = VFMERGE ;
      tree->ast_node.vfmerge.vs2 = vs2;
      tree->ast_node.vfmerge.rs1 = rs1;
      tree->ast_node.vfmerge.vd = vd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x0000000002000000)>>25 == 0x1) && ((binary_stream & 0x00000000FC000000)>>26 == 0x17)) { 
      uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = VFMV ;
      tree->ast_node.vfmv.rs1 = rs1;
      tree->ast_node.vfmv.vd = vd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x5) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x0000000002000000)>>25 == 0x1) && ((binary_stream & 0x00000000FC000000)>>26 == 0x10)) { 
      uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      tree->ast_node_type = VFMVSF ;
      tree->ast_node.vfmvsf.rs1 = rs1;
      tree->ast_node.vfmvsf.vd = vd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x07) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x000000000C000000)>>26 == 0x0) && ((binary_stream & 0x0000000010000000)>>28 == 0x0)) { 
      uint64_t width = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                width = VLE64 ;
        break; 
        case 0x5: 
                width = VLE16 ;
        break; 
        case 0x6: 
                width = VLE32 ;
        break; 
        case 0x0: 
                width = VLE8 ;
        break; 
 
      }
      if (width != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          uint64_t nf = (binary_stream & 0x00000000E0000000)>>29 ;
          tree->ast_node_type = VLSEGTYPE ;
          tree->ast_node.vlsegtype.nf = nf;
          tree->ast_node.vlsegtype.vm = vm;
          tree->ast_node.vlsegtype.rs1 = rs1;
          tree->ast_node.vlsegtype.width = width;
          tree->ast_node.vlsegtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x07) && ((binary_stream & 0x0000000001F00000)>>20 == 0x10) && ((binary_stream & 0x000000000C000000)>>26 == 0x0) && ((binary_stream & 0x0000000010000000)>>28 == 0x0)) { 
      uint64_t width = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                width = VLE64 ;
        break; 
        case 0x5: 
                width = VLE16 ;
        break; 
        case 0x6: 
                width = VLE32 ;
        break; 
        case 0x0: 
                width = VLE8 ;
        break; 
 
      }
      if (width != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          uint64_t nf = (binary_stream & 0x00000000E0000000)>>29 ;
          tree->ast_node_type = VLSEGFFTYPE ;
          tree->ast_node.vlsegfftype.nf = nf;
          tree->ast_node.vlsegfftype.vm = vm;
          tree->ast_node.vlsegfftype.rs1 = rs1;
          tree->ast_node.vlsegfftype.width = width;
          tree->ast_node.vlsegfftype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x27) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x000000000C000000)>>26 == 0x0) && ((binary_stream & 0x0000000010000000)>>28 == 0x0)) { 
      uint64_t width = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                width = VLE64 ;
        break; 
        case 0x5: 
                width = VLE16 ;
        break; 
        case 0x6: 
                width = VLE32 ;
        break; 
        case 0x0: 
                width = VLE8 ;
        break; 
 
      }
      if (width != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vs3 = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          uint64_t nf = (binary_stream & 0x00000000E0000000)>>29 ;
          tree->ast_node_type = VSSEGTYPE ;
          tree->ast_node.vssegtype.nf = nf;
          tree->ast_node.vssegtype.vm = vm;
          tree->ast_node.vssegtype.rs1 = rs1;
          tree->ast_node.vssegtype.width = width;
          tree->ast_node.vssegtype.vs3 = vs3;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x07) && ((binary_stream & 0x000000000C000000)>>26 == 0x2) && ((binary_stream & 0x0000000010000000)>>28 == 0x0)) { 
      uint64_t width = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                width = VLE64 ;
        break; 
        case 0x5: 
                width = VLE16 ;
        break; 
        case 0x6: 
                width = VLE32 ;
        break; 
        case 0x0: 
                width = VLE8 ;
        break; 
 
      }
      if (width != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          uint64_t nf = (binary_stream & 0x00000000E0000000)>>29 ;
          tree->ast_node_type = VLSSEGTYPE ;
          tree->ast_node.vlssegtype.nf = nf;
          tree->ast_node.vlssegtype.vm = vm;
          tree->ast_node.vlssegtype.rs2 = rs2;
          tree->ast_node.vlssegtype.rs1 = rs1;
          tree->ast_node.vlssegtype.width = width;
          tree->ast_node.vlssegtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x27) && ((binary_stream & 0x000000000C000000)>>26 == 0x2) && ((binary_stream & 0x0000000010000000)>>28 == 0x0)) { 
      uint64_t width = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                width = VLE64 ;
        break; 
        case 0x5: 
                width = VLE16 ;
        break; 
        case 0x6: 
                width = VLE32 ;
        break; 
        case 0x0: 
                width = VLE8 ;
        break; 
 
      }
      if (width != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vs3 = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t rs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          uint64_t nf = (binary_stream & 0x00000000E0000000)>>29 ;
          tree->ast_node_type = VSSSEGTYPE ;
          tree->ast_node.vsssegtype.nf = nf;
          tree->ast_node.vsssegtype.vm = vm;
          tree->ast_node.vsssegtype.rs2 = rs2;
          tree->ast_node.vsssegtype.rs1 = rs1;
          tree->ast_node.vsssegtype.width = width;
          tree->ast_node.vsssegtype.vs3 = vs3;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x07) && ((binary_stream & 0x000000000C000000)>>26 == 0x1) && ((binary_stream & 0x0000000010000000)>>28 == 0x0)) { 
      uint64_t width = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                width = VLE64 ;
        break; 
        case 0x5: 
                width = VLE16 ;
        break; 
        case 0x6: 
                width = VLE32 ;
        break; 
        case 0x0: 
                width = VLE8 ;
        break; 
 
      }
      if (width != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          uint64_t nf = (binary_stream & 0x00000000E0000000)>>29 ;
          tree->ast_node_type = VLUXSEGTYPE ;
          tree->ast_node.vluxsegtype.nf = nf;
          tree->ast_node.vluxsegtype.vm = vm;
          tree->ast_node.vluxsegtype.vs2 = vs2;
          tree->ast_node.vluxsegtype.rs1 = rs1;
          tree->ast_node.vluxsegtype.width = width;
          tree->ast_node.vluxsegtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x07) && ((binary_stream & 0x000000000C000000)>>26 == 0x3) && ((binary_stream & 0x0000000010000000)>>28 == 0x0)) { 
      uint64_t width = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                width = VLE64 ;
        break; 
        case 0x5: 
                width = VLE16 ;
        break; 
        case 0x6: 
                width = VLE32 ;
        break; 
        case 0x0: 
                width = VLE8 ;
        break; 
 
      }
      if (width != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          uint64_t nf = (binary_stream & 0x00000000E0000000)>>29 ;
          tree->ast_node_type = VLOXSEGTYPE ;
          tree->ast_node.vloxsegtype.nf = nf;
          tree->ast_node.vloxsegtype.vm = vm;
          tree->ast_node.vloxsegtype.vs2 = vs2;
          tree->ast_node.vloxsegtype.rs1 = rs1;
          tree->ast_node.vloxsegtype.width = width;
          tree->ast_node.vloxsegtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x27) && ((binary_stream & 0x000000000C000000)>>26 == 0x1) && ((binary_stream & 0x0000000010000000)>>28 == 0x0)) { 
      uint64_t width = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                width = VLE64 ;
        break; 
        case 0x5: 
                width = VLE16 ;
        break; 
        case 0x6: 
                width = VLE32 ;
        break; 
        case 0x0: 
                width = VLE8 ;
        break; 
 
      }
      if (width != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vs3 = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          uint64_t nf = (binary_stream & 0x00000000E0000000)>>29 ;
          tree->ast_node_type = VSUXSEGTYPE ;
          tree->ast_node.vsuxsegtype.nf = nf;
          tree->ast_node.vsuxsegtype.vm = vm;
          tree->ast_node.vsuxsegtype.vs2 = vs2;
          tree->ast_node.vsuxsegtype.rs1 = rs1;
          tree->ast_node.vsuxsegtype.width = width;
          tree->ast_node.vsuxsegtype.vs3 = vs3;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x27) && ((binary_stream & 0x000000000C000000)>>26 == 0x3) && ((binary_stream & 0x0000000010000000)>>28 == 0x0)) { 
      uint64_t width = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                width = VLE64 ;
        break; 
        case 0x5: 
                width = VLE16 ;
        break; 
        case 0x6: 
                width = VLE32 ;
        break; 
        case 0x0: 
                width = VLE8 ;
        break; 
 
      }
      if (width != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vs3 = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          uint64_t nf = (binary_stream & 0x00000000E0000000)>>29 ;
          tree->ast_node_type = VSOXSEGTYPE ;
          tree->ast_node.vsoxsegtype.nf = nf;
          tree->ast_node.vsoxsegtype.vm = vm;
          tree->ast_node.vsoxsegtype.vs2 = vs2;
          tree->ast_node.vsoxsegtype.rs1 = rs1;
          tree->ast_node.vsoxsegtype.width = width;
          tree->ast_node.vsoxsegtype.vs3 = vs3;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x07) && ((binary_stream & 0x0000000001F00000)>>20 == 0x08) && ((binary_stream & 0x0000000002000000)>>25 == 0x1) && ((binary_stream & 0x000000000C000000)>>26 == 0x0) && ((binary_stream & 0x0000000010000000)>>28 == 0x0)) { 
      uint64_t width = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x0000000000007000)>>12) {
        case 0x7: 
                width = VLE64 ;
        break; 
        case 0x5: 
                width = VLE16 ;
        break; 
        case 0x6: 
                width = VLE32 ;
        break; 
        case 0x0: 
                width = VLE8 ;
        break; 
 
      }
      if (width != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t nf = (binary_stream & 0x00000000E0000000)>>29 ;
          tree->ast_node_type = VLRETYPE ;
          tree->ast_node.vlretype.nf = nf;
          tree->ast_node.vlretype.rs1 = rs1;
          tree->ast_node.vlretype.width = width;
          tree->ast_node.vlretype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x27) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x0000000001F00000)>>20 == 0x08) && ((binary_stream & 0x0000000002000000)>>25 == 0x1) && ((binary_stream & 0x000000000C000000)>>26 == 0x0) && ((binary_stream & 0x0000000010000000)>>28 == 0x0)) { 
      uint64_t vs3 = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
      uint64_t nf = (binary_stream & 0x00000000E0000000)>>29 ;
      tree->ast_node_type = VSRETYPE ;
      tree->ast_node.vsretype.nf = nf;
      tree->ast_node.vsretype.rs1 = rs1;
      tree->ast_node.vsretype.vs3 = vs3;
      return ;
  }
  if (((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x0000000001F00000)>>20 == 0x0B) && ((binary_stream & 0x0000000002000000)>>25 == 0x1) && ((binary_stream & 0x000000000C000000)>>26 == 0x0) && ((binary_stream & 0x0000000010000000)>>28 == 0x0) && ((binary_stream & 0x00000000E0000000)>>29 == 0x0)) { 
      uint64_t op = 0xFFFFFFFFFFFFFFFF ;
      switch (binary_stream & 0x000000000000007F) {
        case 0x07: 
                op = VLM ;
        break; 
        case 0x27: 
                op = VSM ;
        break; 
 
      }
      if (op != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd_or_vs3 = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          tree->ast_node_type = VMTYPE ;
          tree->ast_node.vmtype.rs1 = rs1;
          tree->ast_node.vmtype.vd_or_vs3 = vd_or_vs3;
          tree->ast_node.vmtype.op = op;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x0000000002000000)>>25 == 0x1)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x19: 
                funct6 = MM_VMAND ;
        break; 
        case 0x1B: 
                funct6 = MM_VMXOR ;
        break; 
        case 0x1C: 
                funct6 = MM_VMORN ;
        break; 
        case 0x1A: 
                funct6 = MM_VMOR ;
        break; 
        case 0x1E: 
                funct6 = MM_VMNOR ;
        break; 
        case 0x1D: 
                funct6 = MM_VMNAND ;
        break; 
        case 0x18: 
                funct6 = MM_VMANDN ;
        break; 
        case 0x1F: 
                funct6 = MM_VMXNOR ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = MMTYPE ;
          tree->ast_node.mmtype.funct6 = funct6;
          tree->ast_node.mmtype.vs2 = vs2;
          tree->ast_node.mmtype.vs1 = vs1;
          tree->ast_node.mmtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000000F8000)>>15 == 0x10) && ((binary_stream & 0x00000000FC000000)>>26 == 0x10)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
      tree->ast_node_type = VCPOP_M ;
      tree->ast_node.vcpop_m.vm = vm;
      tree->ast_node.vcpop_m.vs2 = vs2;
      tree->ast_node.vcpop_m.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000000F8000)>>15 == 0x11) && ((binary_stream & 0x00000000FC000000)>>26 == 0x10)) { 
      uint64_t rd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
      tree->ast_node_type = VFIRST_M ;
      tree->ast_node.vfirst_m.vm = vm;
      tree->ast_node.vfirst_m.vs2 = vs2;
      tree->ast_node.vfirst_m.rd = rd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000000F8000)>>15 == 0x01) && ((binary_stream & 0x00000000FC000000)>>26 == 0x14)) { 
      uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
      tree->ast_node_type = VMSBF_M ;
      tree->ast_node.vmsbf_m.vm = vm;
      tree->ast_node.vmsbf_m.vs2 = vs2;
      tree->ast_node.vmsbf_m.vd = vd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000000F8000)>>15 == 0x03) && ((binary_stream & 0x00000000FC000000)>>26 == 0x14)) { 
      uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
      tree->ast_node_type = VMSIF_M ;
      tree->ast_node.vmsif_m.vm = vm;
      tree->ast_node.vmsif_m.vs2 = vs2;
      tree->ast_node.vmsif_m.vd = vd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000000F8000)>>15 == 0x02) && ((binary_stream & 0x00000000FC000000)>>26 == 0x14)) { 
      uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
      tree->ast_node_type = VMSOF_M ;
      tree->ast_node.vmsof_m.vm = vm;
      tree->ast_node.vmsof_m.vs2 = vs2;
      tree->ast_node.vmsof_m.vd = vd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000000F8000)>>15 == 0x10) && ((binary_stream & 0x00000000FC000000)>>26 == 0x14)) { 
      uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
      uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
      tree->ast_node_type = VIOTA_M ;
      tree->ast_node.viota_m.vm = vm;
      tree->ast_node.viota_m.vs2 = vs2;
      tree->ast_node.viota_m.vd = vd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x2) && ((binary_stream & 0x00000000000F8000)>>15 == 0x11) && ((binary_stream & 0x0000000001F00000)>>20 == 0x00) && ((binary_stream & 0x00000000FC000000)>>26 == 0x14)) { 
      uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
      uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
      tree->ast_node_type = VID_V ;
      tree->ast_node.vid_v.vm = vm;
      tree->ast_node.vid_v.vd = vd;
      return ;
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x0000000002000000)>>25 == 0x0)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x13: 
                funct6 = VVM_VMSBC ;
        break; 
        case 0x11: 
                funct6 = VVM_VMADC ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = VVMTYPE ;
          tree->ast_node.vvmtype.funct6 = funct6;
          tree->ast_node.vvmtype.vs2 = vs2;
          tree->ast_node.vvmtype.vs1 = vs1;
          tree->ast_node.vvmtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x0000000002000000)>>25 == 0x1)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x13: 
                funct6 = VVMC_VMSBC ;
        break; 
        case 0x11: 
                funct6 = VVMC_VMADC ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = VVMCTYPE ;
          tree->ast_node.vvmctype.funct6 = funct6;
          tree->ast_node.vvmctype.vs2 = vs2;
          tree->ast_node.vvmctype.vs1 = vs1;
          tree->ast_node.vvmctype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x0) && ((binary_stream & 0x0000000002000000)>>25 == 0x0)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x10: 
                funct6 = VVMS_VADC ;
        break; 
        case 0x12: 
                funct6 = VVMS_VSBC ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = VVMSTYPE ;
          tree->ast_node.vvmstype.funct6 = funct6;
          tree->ast_node.vvmstype.vs2 = vs2;
          tree->ast_node.vvmstype.vs1 = vs1;
          tree->ast_node.vvmstype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x0)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x19: 
                funct6 = VVCMP_VMSNE ;
        break; 
        case 0x1B: 
                funct6 = VVCMP_VMSLT ;
        break; 
        case 0x1C: 
                funct6 = VVCMP_VMSLEU ;
        break; 
        case 0x1A: 
                funct6 = VVCMP_VMSLTU ;
        break; 
        case 0x18: 
                funct6 = VVCMP_VMSEQ ;
        break; 
        case 0x1D: 
                funct6 = VVCMP_VMSLE ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = VVCMPTYPE ;
          tree->ast_node.vvcmptype.funct6 = funct6;
          tree->ast_node.vvcmptype.vm = vm;
          tree->ast_node.vvcmptype.vs2 = vs2;
          tree->ast_node.vvcmptype.vs1 = vs1;
          tree->ast_node.vvcmptype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x4) && ((binary_stream & 0x0000000002000000)>>25 == 0x0)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x13: 
                funct6 = VXM_VMSBC ;
        break; 
        case 0x11: 
                funct6 = VXM_VMADC ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = VXMTYPE ;
          tree->ast_node.vxmtype.funct6 = funct6;
          tree->ast_node.vxmtype.vs2 = vs2;
          tree->ast_node.vxmtype.rs1 = rs1;
          tree->ast_node.vxmtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x4) && ((binary_stream & 0x0000000002000000)>>25 == 0x1)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x13: 
                funct6 = VXMC_VMSBC ;
        break; 
        case 0x11: 
                funct6 = VXMC_VMADC ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = VXMCTYPE ;
          tree->ast_node.vxmctype.funct6 = funct6;
          tree->ast_node.vxmctype.vs2 = vs2;
          tree->ast_node.vxmctype.rs1 = rs1;
          tree->ast_node.vxmctype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x4) && ((binary_stream & 0x0000000002000000)>>25 == 0x0)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x10: 
                funct6 = VXMS_VADC ;
        break; 
        case 0x12: 
                funct6 = VXMS_VSBC ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = VXMSTYPE ;
          tree->ast_node.vxmstype.funct6 = funct6;
          tree->ast_node.vxmstype.vs2 = vs2;
          tree->ast_node.vxmstype.rs1 = rs1;
          tree->ast_node.vxmstype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x4)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x19: 
                funct6 = VXCMP_VMSNE ;
        break; 
        case 0x1B: 
                funct6 = VXCMP_VMSLT ;
        break; 
        case 0x1C: 
                funct6 = VXCMP_VMSLEU ;
        break; 
        case 0x1A: 
                funct6 = VXCMP_VMSLTU ;
        break; 
        case 0x1E: 
                funct6 = VXCMP_VMSGTU ;
        break; 
        case 0x18: 
                funct6 = VXCMP_VMSEQ ;
        break; 
        case 0x1D: 
                funct6 = VXCMP_VMSLE ;
        break; 
        case 0x1F: 
                funct6 = VXCMP_VMSGT ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = VXCMPTYPE ;
          tree->ast_node.vxcmptype.funct6 = funct6;
          tree->ast_node.vxcmptype.vm = vm;
          tree->ast_node.vxcmptype.vs2 = vs2;
          tree->ast_node.vxcmptype.rs1 = rs1;
          tree->ast_node.vxcmptype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x3) && ((binary_stream & 0x0000000002000000)>>25 == 0x0)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x11: 
                funct6 = VIM_VMADC ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t simm = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = VIMTYPE ;
          tree->ast_node.vimtype.funct6 = funct6;
          tree->ast_node.vimtype.vs2 = vs2;
          tree->ast_node.vimtype.simm = simm;
          tree->ast_node.vimtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x3) && ((binary_stream & 0x0000000002000000)>>25 == 0x1)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x11: 
                funct6 = VIMC_VMADC ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t simm = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = VIMCTYPE ;
          tree->ast_node.vimctype.funct6 = funct6;
          tree->ast_node.vimctype.vs2 = vs2;
          tree->ast_node.vimctype.simm = simm;
          tree->ast_node.vimctype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x3) && ((binary_stream & 0x0000000002000000)>>25 == 0x0)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x10: 
                funct6 = VIMS_VADC ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t simm = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          tree->ast_node_type = VIMSTYPE ;
          tree->ast_node.vimstype.funct6 = funct6;
          tree->ast_node.vimstype.vs2 = vs2;
          tree->ast_node.vimstype.simm = simm;
          tree->ast_node.vimstype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x3)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x19: 
                funct6 = VICMP_VMSNE ;
        break; 
        case 0x1C: 
                funct6 = VICMP_VMSLEU ;
        break; 
        case 0x1E: 
                funct6 = VICMP_VMSGTU ;
        break; 
        case 0x18: 
                funct6 = VICMP_VMSEQ ;
        break; 
        case 0x1D: 
                funct6 = VICMP_VMSLE ;
        break; 
        case 0x1F: 
                funct6 = VICMP_VMSGT ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t simm = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = VICMPTYPE ;
          tree->ast_node.vicmptype.funct6 = funct6;
          tree->ast_node.vicmptype.vm = vm;
          tree->ast_node.vicmptype.vs2 = vs2;
          tree->ast_node.vicmptype.simm = simm;
          tree->ast_node.vicmptype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x1)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x19: 
                funct6 = FVVM_VMFLE ;
        break; 
        case 0x1B: 
                funct6 = FVVM_VMFLT ;
        break; 
        case 0x1C: 
                funct6 = FVVM_VMFNE ;
        break; 
        case 0x18: 
                funct6 = FVVM_VMFEQ ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = FVVMTYPE ;
          tree->ast_node.fvvmtype.funct6 = funct6;
          tree->ast_node.fvvmtype.vm = vm;
          tree->ast_node.fvvmtype.vs2 = vs2;
          tree->ast_node.fvvmtype.vs1 = vs1;
          tree->ast_node.fvvmtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x5)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x19: 
                funct6 = VFM_VMFLE ;
        break; 
        case 0x1B: 
                funct6 = VFM_VMFLT ;
        break; 
        case 0x1C: 
                funct6 = VFM_VMFNE ;
        break; 
        case 0x18: 
                funct6 = VFM_VMFEQ ;
        break; 
        case 0x1D: 
                funct6 = VFM_VMFGT ;
        break; 
        case 0x1F: 
                funct6 = VFM_VMFGE ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t rs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = FVFMTYPE ;
          tree->ast_node.fvfmtype.funct6 = funct6;
          tree->ast_node.fvfmtype.vm = vm;
          tree->ast_node.fvfmtype.vs2 = vs2;
          tree->ast_node.fvfmtype.rs1 = rs1;
          tree->ast_node.fvfmtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x0)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x31: 
                funct6 = IVV_VWREDSUM ;
        break; 
        case 0x30: 
                funct6 = IVV_VWREDSUMU ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = RIVVTYPE ;
          tree->ast_node.rivvtype.funct6 = funct6;
          tree->ast_node.rivvtype.vm = vm;
          tree->ast_node.rivvtype.vs2 = vs2;
          tree->ast_node.rivvtype.vs1 = vs1;
          tree->ast_node.rivvtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x2)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x01: 
                funct6 = MVV_VREDAND ;
        break; 
        case 0x06: 
                funct6 = MVV_VREDMAXU ;
        break; 
        case 0x07: 
                funct6 = MVV_VREDMAX ;
        break; 
        case 0x04: 
                funct6 = MVV_VREDMINU ;
        break; 
        case 0x00: 
                funct6 = MVV_VREDSUM ;
        break; 
        case 0x03: 
                funct6 = MVV_VREDXOR ;
        break; 
        case 0x05: 
                funct6 = MVV_VREDMIN ;
        break; 
        case 0x02: 
                funct6 = MVV_VREDOR ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = RMVVTYPE ;
          tree->ast_node.rmvvtype.funct6 = funct6;
          tree->ast_node.rmvvtype.vm = vm;
          tree->ast_node.rmvvtype.vs2 = vs2;
          tree->ast_node.rmvvtype.vs1 = vs1;
          tree->ast_node.rmvvtype.vd = vd;
          return ;
      }
  }
  if ((binary_stream & 0x000000000000007F == 0x57) && ((binary_stream & 0x0000000000007000)>>12 == 0x1)) { 
      uint64_t funct6 = 0xFFFFFFFFFFFFFFFF ;
      switch ((binary_stream & 0x00000000FC000000)>>26) {
        case 0x33: 
                funct6 = FVV_VFWREDOSUM ;
        break; 
        case 0x01: 
                funct6 = FVV_VFREDUSUM ;
        break; 
        case 0x31: 
                funct6 = FVV_VFWREDUSUM ;
        break; 
        case 0x07: 
                funct6 = FVV_VFREDMAX ;
        break; 
        case 0x03: 
                funct6 = FVV_VFREDOSUM ;
        break; 
        case 0x05: 
                funct6 = FVV_VFREDMIN ;
        break; 
 
      }
      if (funct6 != 0xFFFFFFFFFFFFFFFF) { 
          uint64_t vd = (binary_stream & 0x0000000000000F80)>>7 ;
          uint64_t vs1 = (binary_stream & 0x00000000000F8000)>>15 ;
          uint64_t vs2 = (binary_stream & 0x0000000001F00000)>>20 ;
          uint64_t vm = (binary_stream & 0x0000000002000000)>>25 ;
          tree->ast_node_type = RFVVTYPE ;
          tree->ast_node.rfvvtype.funct6 = funct6;
          tree->ast_node.rfvvtype.vm = vm;
          tree->ast_node.rfvvtype.vs2 = vs2;
          tree->ast_node.rfvvtype.vs1 = vs1;
          tree->ast_node.rfvvtype.vd = vd;
          return ;
      }
  }
    uint64_t s = binary_stream & 0x00000000FFFFFFFF ;
    tree->ast_node_type = ILLEGAL ;
    tree->ast_node.illegal = s;
    return ;
}