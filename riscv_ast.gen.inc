#include <stdint.h>



enum {false = 0, true = 1}; 
struct ast {
  enum  {
    ILLEGAL,
    C_ILLEGAL,
    UTYPE,
    RISCV_JAL,
    RISCV_JALR,
    BTYPE,
    ITYPE,
    SHIFTIOP,
    RTYPE,
    LOAD,
    STORE,
    ADDIW,
    RTYPEW,
    SHIFTIWOP,
    FENCE,
    FENCE_TSO,
    FENCEI,
    ECALL,
    MRET,
    SRET,
    EBREAK,
    WFI,
    SFENCE_VMA,
    LOADRES,
    STORECON,
    AMO,
    C_NOP,
    C_ADDI4SPN,
    C_LW,
    C_LD,
    C_SW,
    C_SD,
    C_ADDI,
    C_JAL,
    C_ADDIW,
    C_LI,
    C_ADDI16SP,
    C_LUI,
    C_SRLI,
    C_SRAI,
    C_ANDI,
    C_SUB,
    C_XOR,
    C_OR,
    C_AND,
    C_SUBW,
    C_ADDW,
    C_J,
    C_BEQZ,
    C_BNEZ,
    C_SLLI,
    C_LWSP,
    C_LDSP,
    C_SWSP,
    C_SDSP,
    C_JR,
    C_JALR,
    C_MV,
    C_EBREAK,
    C_ADD,
    MUL,
    DIV,
    REM,
    MULW,
    DIVW,
    REMW,
    CSR,
    URET,
    C_NOP_HINT,
    C_ADDI_HINT,
    C_LI_HINT,
    C_LUI_HINT,
    C_MV_HINT,
    C_ADD_HINT,
    C_SLLI_HINT,
    C_SRLI_HINT,
    C_SRAI_HINT,
    FENCE_RESERVED,
    FENCEI_RESERVED,
    LOAD_FP,
    STORE_FP,
    F_MADD_TYPE_S,
    F_BIN_RM_TYPE_S,
    F_UN_RM_TYPE_S,
    F_BIN_TYPE_S,
    F_UN_TYPE_S,
    C_FLWSP,
    C_FSWSP,
    C_FLW,
    C_FSW,
    F_MADD_TYPE_D,
    F_BIN_RM_TYPE_D,
    F_UN_RM_TYPE_D,
    F_BIN_TYPE_D,
    F_UN_TYPE_D,
    C_FLDSP,
    C_FSDSP,
    C_FLD,
    C_FSD,
    SINVAL_VMA,
    SFENCE_W_INVAL,
    SFENCE_INVAL_IR,
    RISCV_SLLIUW,
    ZBA_RTYPEUW,
    ZBA_RTYPE,
    RISCV_RORIW,
    RISCV_RORI,
    ZBB_RTYPEW,
    ZBB_RTYPE,
    ZBB_EXTOP,
    RISCV_REV8,
    RISCV_ORCB,
    RISCV_CPOP,
    RISCV_CPOPW,
    RISCV_CLZ,
    RISCV_CLZW,
    RISCV_CTZ,
    RISCV_CTZW,
    RISCV_CLMUL,
    RISCV_CLMULH,
    RISCV_CLMULR,
    ZBS_IOP,
    ZBS_RTYPE,
    C_LBU,
    C_LHU,
    C_LH,
    C_SB,
    C_SH,
    C_ZEXT_B,
    C_SEXT_B,
    C_ZEXT_H,
    C_SEXT_H,
    C_ZEXT_W,
    C_NOT,
    C_MUL,
    F_BIN_RM_TYPE_H,
    F_MADD_TYPE_H,
    F_BIN_TYPE_H,
    F_UN_RM_TYPE_H,
    F_UN_TYPE_H,
    RISCV_FLI_H,
    RISCV_FLI_S,
    RISCV_FLI_D,
    RISCV_FMINM_H,
    RISCV_FMAXM_H,
    RISCV_FMINM_S,
    RISCV_FMAXM_S,
    RISCV_FMINM_D,
    RISCV_FMAXM_D,
    RISCV_FROUND_H,
    RISCV_FROUNDNX_H,
    RISCV_FROUND_S,
    RISCV_FROUNDNX_S,
    RISCV_FROUND_D,
    RISCV_FROUNDNX_D,
    RISCV_FMVH_X_D,
    RISCV_FMVP_D_X,
    RISCV_FLEQ_H,
    RISCV_FLTQ_H,
    RISCV_FLEQ_S,
    RISCV_FLTQ_S,
    RISCV_FLEQ_D,
    RISCV_FLTQ_D,
    RISCV_FCVTMOD_W_D,
    SHA256SIG0,
    SHA256SIG1,
    SHA256SUM0,
    SHA256SUM1,
    AES32ESMI,
    AES32ESI,
    AES32DSMI,
    AES32DSI,
    SHA512SIG0L,
    SHA512SIG0H,
    SHA512SIG1L,
    SHA512SIG1H,
    SHA512SUM0R,
    SHA512SUM1R,
    AES64KS1I,
    AES64KS2,
    AES64IM,
    AES64ESM,
    AES64ES,
    AES64DSM,
    AES64DS,
    SHA512SIG0,
    SHA512SIG1,
    SHA512SUM0,
    SHA512SUM1,
    SM3P0,
    SM3P1,
    SM4ED,
    SM4KS,
    ZBKB_RTYPE,
    ZBKB_PACKW,
    RISCV_ZIP,
    RISCV_UNZIP,
    RISCV_BREV8,
    RISCV_XPERM8,
    RISCV_XPERM4,
    ZICOND_RTYPE,
    VSETVLI,
    VSETVL,
    VSETIVLI,
    VVTYPE,
    NVSTYPE,
    NVTYPE,
    MASKTYPEV,
    MOVETYPEV,
    VXTYPE,
    NXSTYPE,
    NXTYPE,
    VXSG,
    MASKTYPEX,
    MOVETYPEX,
    VITYPE,
    NISTYPE,
    NITYPE,
    VISG,
    MASKTYPEI,
    MOVETYPEI,
    VMVRTYPE,
    MVVTYPE,
    MVVMATYPE,
    WVVTYPE,
    WVTYPE,
    WMVVTYPE,
    VEXT2TYPE,
    VEXT4TYPE,
    VEXT8TYPE,
    VMVXS,
    MVVCOMPRESS,
    MVXTYPE,
    MVXMATYPE,
    WVXTYPE,
    WXTYPE,
    WMVXTYPE,
    VMVSX,
    FVVTYPE,
    FVVMATYPE,
    FWVVTYPE,
    FWVVMATYPE,
    FWVTYPE,
    VFUNARY0,
    VFWUNARY0,
    VFNUNARY0,
    VFUNARY1,
    VFMVFS,
    FVFTYPE,
    FVFMATYPE,
    FWVFTYPE,
    FWVFMATYPE,
    FWFTYPE,
    VFMERGE,
    VFMV,
    VFMVSF,
    VLSEGTYPE,
    VLSEGFFTYPE,
    VSSEGTYPE,
    VLSSEGTYPE,
    VSSSEGTYPE,
    VLUXSEGTYPE,
    VLOXSEGTYPE,
    VSUXSEGTYPE,
    VSOXSEGTYPE,
    VLRETYPE,
    VSRETYPE,
    VMTYPE,
    MMTYPE,
    VCPOP_M,
    VFIRST_M,
    VMSBF_M,
    VMSIF_M,
    VMSOF_M,
    VIOTA_M,
    VID_V,
    VVMTYPE,
    VVMCTYPE,
    VVMSTYPE,
    VVCMPTYPE,
    VXMTYPE,
    VXMCTYPE,
    VXMSTYPE,
    VXCMPTYPE,
    VIMTYPE,
    VIMCTYPE,
    VIMSTYPE,
    VICMPTYPE,
    FVVMTYPE,
    FVFMTYPE,
    RIVVTYPE,
    RMVVTYPE,
    RFVVTYPE,
    RISCV_ZICBOM,
    RISCV_ZICBOZ
  } ast_node_type;

  union  {
    uint32_t illegal;
    uint16_t c_illegal;
    struct  {
      uint32_t imm /* bits : 20 */;
      uint8_t rd /* bits : 5 */;
      enum uop {
        RISCV_LUI,
        RISCV_AUIPC
      } op;

    } utype;

    struct  {
      uint32_t imm /* bits : 21 */;
      uint8_t rd /* bits : 5 */;
    } riscv_jal;

    struct  {
      uint16_t imm /* bits : 12 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_jalr;

    struct  {
      uint16_t imm /* bits : 13 */;
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      enum bop {
        RISCV_BEQ,
        RISCV_BNE,
        RISCV_BLT,
        RISCV_BGE,
        RISCV_BLTU,
        RISCV_BGEU
      } op;

    } btype;

    struct  {
      uint16_t imm /* bits : 12 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      enum iop {
        RISCV_ADDI,
        RISCV_SLTI,
        RISCV_SLTIU,
        RISCV_XORI,
        RISCV_ORI,
        RISCV_ANDI
      } op;

    } itype;

    struct  {
      uint8_t shamt /* bits : 6 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      enum sop {
        RISCV_SLLI,
        RISCV_SRLI,
        RISCV_SRAI
      } op;

    } shiftiop;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      enum rop {
        RISCV_ADD,
        RISCV_SUB,
        RISCV_SLL,
        RISCV_SLT,
        RISCV_SLTU,
        RISCV_XOR,
        RISCV_SRL,
        RISCV_SRA,
        RISCV_OR,
        RISCV_AND
      } op;

    } rtype;

    struct  {
      uint16_t imm /* bits : 12 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      uint8_t is_unsigned /* bits : 1 */;
      enum word_width {
        BYTE,
        HALF,
        WORD,
        DOUBLE
      } width;

      uint8_t aq /* bits : 1 */;
      uint8_t rl /* bits : 1 */;
    } load;

    struct  {
      uint16_t imm /* bits : 12 */;
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      enum word_width width;
      uint8_t aq /* bits : 1 */;
      uint8_t rl /* bits : 1 */;
    } store;

    struct  {
      uint16_t imm /* bits : 12 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } addiw;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      enum ropw {
        RISCV_ADDW,
        RISCV_SUBW,
        RISCV_SLLW,
        RISCV_SRLW,
        RISCV_SRAW
      } op;

    } rtypew;

    struct  {
      uint8_t shamt /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      enum sopw {
        RISCV_SLLIW,
        RISCV_SRLIW,
        RISCV_SRAIW
      } op;

    } shiftiwop;

    struct  {
      uint8_t pred /* bits : 4 */;
      uint8_t succ /* bits : 4 */;
    } fence;

    struct  {
      uint8_t pred /* bits : 4 */;
      uint8_t succ /* bits : 4 */;
    } fence_tso;

    
    
    
    
    
    
    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rs2 /* bits : 5 */;
    } sfence_vma;

    struct  {
      uint8_t aq /* bits : 1 */;
      uint8_t rl /* bits : 1 */;
      uint8_t rs1 /* bits : 5 */;
      enum word_width width;
      uint8_t rd /* bits : 5 */;
    } loadres;

    struct  {
      uint8_t aq /* bits : 1 */;
      uint8_t rl /* bits : 1 */;
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      enum word_width width;
      uint8_t rd /* bits : 5 */;
    } storecon;

    struct  {
      enum amoop {
        AMOSWAP,
        AMOADD,
        AMOXOR,
        AMOAND,
        AMOOR,
        AMOMIN,
        AMOMAX,
        AMOMINU,
        AMOMAXU
      } op;

      uint8_t aq /* bits : 1 */;
      uint8_t rl /* bits : 1 */;
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      enum word_width width;
      uint8_t rd /* bits : 5 */;
    } amo;

    
    struct  {
      uint8_t rdc /* bits : 3 */;
      uint8_t nzimm;
    } c_addi4spn;

    struct  {
      uint8_t uimm /* bits : 5 */;
      uint8_t rsc /* bits : 3 */;
      uint8_t rdc /* bits : 3 */;
    } c_lw;

    struct  {
      uint8_t uimm /* bits : 5 */;
      uint8_t rsc /* bits : 3 */;
      uint8_t rdc /* bits : 3 */;
    } c_ld;

    struct  {
      uint8_t uimm /* bits : 5 */;
      uint8_t rsc1 /* bits : 3 */;
      uint8_t rsc2 /* bits : 3 */;
    } c_sw;

    struct  {
      uint8_t uimm /* bits : 5 */;
      uint8_t rsc1 /* bits : 3 */;
      uint8_t rsc2 /* bits : 3 */;
    } c_sd;

    struct  {
      uint8_t nzi /* bits : 6 */;
      uint8_t rsd /* bits : 5 */;
    } c_addi;

    uint16_t c_jal /* bits : 11 */;
    struct  {
      uint8_t imm /* bits : 6 */;
      uint8_t rsd /* bits : 5 */;
    } c_addiw;

    struct  {
      uint8_t imm /* bits : 6 */;
      uint8_t rd /* bits : 5 */;
    } c_li;

    uint8_t c_addi16sp /* bits : 6 */;
    struct  {
      uint8_t imm /* bits : 6 */;
      uint8_t rd /* bits : 5 */;
    } c_lui;

    struct  {
      uint8_t shamt /* bits : 6 */;
      uint8_t rsd /* bits : 3 */;
    } c_srli;

    struct  {
      uint8_t shamt /* bits : 6 */;
      uint8_t rsd /* bits : 3 */;
    } c_srai;

    struct  {
      uint8_t imm /* bits : 6 */;
      uint8_t rsd /* bits : 3 */;
    } c_andi;

    struct  {
      uint8_t rsd /* bits : 3 */;
      uint8_t rs2 /* bits : 3 */;
    } c_sub;

    struct  {
      uint8_t rsd /* bits : 3 */;
      uint8_t rs2 /* bits : 3 */;
    } c_xor;

    struct  {
      uint8_t rsd /* bits : 3 */;
      uint8_t rs2 /* bits : 3 */;
    } c_or;

    struct  {
      uint8_t rsd /* bits : 3 */;
      uint8_t rs2 /* bits : 3 */;
    } c_and;

    struct  {
      uint8_t rsd /* bits : 3 */;
      uint8_t rs2 /* bits : 3 */;
    } c_subw;

    struct  {
      uint8_t rsd /* bits : 3 */;
      uint8_t rs2 /* bits : 3 */;
    } c_addw;

    uint16_t c_j /* bits : 11 */;
    struct  {
      uint8_t imm;
      uint8_t rs /* bits : 3 */;
    } c_beqz;

    struct  {
      uint8_t imm;
      uint8_t rs /* bits : 3 */;
    } c_bnez;

    struct  {
      uint8_t shamt /* bits : 6 */;
      uint8_t rsd /* bits : 5 */;
    } c_slli;

    struct  {
      uint8_t uimm /* bits : 6 */;
      uint8_t rd /* bits : 5 */;
    } c_lwsp;

    struct  {
      uint8_t uimm /* bits : 6 */;
      uint8_t rd /* bits : 5 */;
    } c_ldsp;

    struct  {
      uint8_t uimm /* bits : 6 */;
      uint8_t rs2 /* bits : 5 */;
    } c_swsp;

    struct  {
      uint8_t uimm /* bits : 6 */;
      uint8_t rs2 /* bits : 5 */;
    } c_sdsp;

    uint8_t c_jr /* bits : 5 */;
    uint8_t c_jalr /* bits : 5 */;
    struct  {
      uint8_t rd /* bits : 5 */;
      uint8_t rs2 /* bits : 5 */;
    } c_mv;

    
    struct  {
      uint8_t rsd /* bits : 5 */;
      uint8_t rs2 /* bits : 5 */;
    } c_add;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      struct mul_op {
        uint8_t high /* bits : 1 */;
        uint8_t signed_rs1 /* bits : 1 */;
        uint8_t signed_rs2 /* bits : 1 */;
      } mul_op;

    } mul;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      uint8_t s /* bits : 1 */;
    } div;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      uint8_t s /* bits : 1 */;
    } rem;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } mulw;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      uint8_t s /* bits : 1 */;
    } divw;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      uint8_t s /* bits : 1 */;
    } remw;

    struct  {
      uint16_t csr /* bits : 12 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      uint8_t is_imm /* bits : 1 */;
      enum csrop {
        CSRRW,
        CSRRS,
        CSRRC
      } op;

    } csr;

    
    uint8_t c_nop_hint /* bits : 6 */;
    uint8_t c_addi_hint /* bits : 5 */;
    uint8_t c_li_hint /* bits : 6 */;
    uint8_t c_lui_hint /* bits : 6 */;
    uint8_t c_mv_hint /* bits : 5 */;
    uint8_t c_add_hint /* bits : 5 */;
    struct  {
      uint8_t shamt /* bits : 6 */;
      uint8_t rsd /* bits : 5 */;
    } c_slli_hint;

    uint8_t c_srli_hint /* bits : 3 */;
    uint8_t c_srai_hint /* bits : 3 */;
    struct  {
      uint8_t fm /* bits : 4 */;
      uint8_t pred /* bits : 4 */;
      uint8_t succ /* bits : 4 */;
      uint8_t rs /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } fence_reserved;

    struct  {
      uint16_t imm /* bits : 12 */;
      uint8_t rs /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } fencei_reserved;

    struct  {
      uint16_t imm /* bits : 12 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      enum word_width width;
    } load_fp;

    struct  {
      uint16_t imm /* bits : 12 */;
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      enum word_width width;
    } store_fp;

    struct  {
      uint8_t rs3 /* bits : 5 */;
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      enum rounding_mode {
        RM_RNE,
        RM_RTZ,
        RM_RDN,
        RM_RUP,
        RM_RMM,
        RM_DYN
      } rm;

      uint8_t rd /* bits : 5 */;
      enum f_madd_op_S {
        FMADD_S,
        FMSUB_S,
        FNMSUB_S,
        FNMADD_S
      } op;

    } f_madd_type_s;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      enum rounding_mode rm;
      uint8_t rd /* bits : 5 */;
      enum f_bin_rm_op_S {
        FADD_S,
        FSUB_S,
        FMUL_S,
        FDIV_S
      } op;

    } f_bin_rm_type_s;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      enum rounding_mode rm;
      uint8_t rd /* bits : 5 */;
      enum f_un_rm_op_S {
        FSQRT_S,
        FCVT_W_S,
        FCVT_WU_S,
        FCVT_S_W,
        FCVT_S_WU,
        FCVT_L_S,
        FCVT_LU_S,
        FCVT_S_L,
        FCVT_S_LU
      } fcvt_s_lu;

    } f_un_rm_type_s;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      enum f_bin_op_S {
        FSGNJ_S,
        FSGNJN_S,
        FSGNJX_S,
        FMIN_S,
        FMAX_S,
        FEQ_S,
        FLT_S,
        FLE_S
      } fle_s;

    } f_bin_type_s;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      enum f_un_op_S {
        FCLASS_S,
        FMV_X_W,
        FMV_W_X
      } fmv_w_x;

    } f_un_type_s;

    struct  {
      uint8_t imm /* bits : 6 */;
      uint8_t rd /* bits : 5 */;
    } c_flwsp;

    struct  {
      uint8_t uimm /* bits : 6 */;
      uint8_t rs2 /* bits : 5 */;
    } c_fswsp;

    struct  {
      uint8_t uimm /* bits : 5 */;
      uint8_t rsc /* bits : 3 */;
      uint8_t rdc /* bits : 3 */;
    } c_flw;

    struct  {
      uint8_t uimm /* bits : 5 */;
      uint8_t rsc1 /* bits : 3 */;
      uint8_t rsc2 /* bits : 3 */;
    } c_fsw;

    struct  {
      uint8_t rs3 /* bits : 5 */;
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      enum rounding_mode rm;
      uint8_t rd /* bits : 5 */;
      enum f_madd_op_D {
        FMADD_D,
        FMSUB_D,
        FNMSUB_D,
        FNMADD_D
      } op;

    } f_madd_type_d;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      enum rounding_mode rm;
      uint8_t rd /* bits : 5 */;
      enum f_bin_rm_op_D {
        FADD_D,
        FSUB_D,
        FMUL_D,
        FDIV_D
      } op;

    } f_bin_rm_type_d;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      enum rounding_mode rm;
      uint8_t rd /* bits : 5 */;
      enum f_un_rm_op_D {
        FSQRT_D,
        FCVT_W_D,
        FCVT_WU_D,
        FCVT_D_W,
        FCVT_D_WU,
        FCVT_S_D,
        FCVT_D_S,
        FCVT_L_D,
        FCVT_LU_D,
        FCVT_D_L,
        FCVT_D_LU
      } fcvt_d_lu;

    } f_un_rm_type_d;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      enum f_bin_op_D {
        FSGNJ_D,
        FSGNJN_D,
        FSGNJX_D,
        FMIN_D,
        FMAX_D,
        FEQ_D,
        FLT_D,
        FLE_D
      } fle_d;

    } f_bin_type_d;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      enum f_un_op_D {
        FCLASS_D,
        FMV_X_D,
        FMV_D_X
      } fmv_d_x;

    } f_un_type_d;

    struct  {
      uint8_t uimm /* bits : 6 */;
      uint8_t rd /* bits : 5 */;
    } c_fldsp;

    struct  {
      uint8_t uimm /* bits : 6 */;
      uint8_t rs2 /* bits : 5 */;
    } c_fsdsp;

    struct  {
      uint8_t uimm /* bits : 5 */;
      uint8_t rsc /* bits : 3 */;
      uint8_t rdc /* bits : 3 */;
    } c_fld;

    struct  {
      uint8_t uimm /* bits : 5 */;
      uint8_t rsc1 /* bits : 3 */;
      uint8_t rsc2 /* bits : 3 */;
    } c_fsd;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rs2 /* bits : 5 */;
    } sinval_vma;

    
    
    struct  {
      uint8_t shamt /* bits : 6 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_slliuw;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      enum bropw_zba {
        RISCV_ADDUW,
        RISCV_SH1ADDUW,
        RISCV_SH2ADDUW,
        RISCV_SH3ADDUW
      } op;

    } zba_rtypeuw;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      enum brop_zba {
        RISCV_SH1ADD,
        RISCV_SH2ADD,
        RISCV_SH3ADD
      } op;

    } zba_rtype;

    struct  {
      uint8_t shamt /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_roriw;

    struct  {
      uint8_t shamt /* bits : 6 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_rori;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      enum bropw_zbb {
        RISCV_ROLW,
        RISCV_RORW
      } op;

    } zbb_rtypew;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      enum brop_zbb {
        RISCV_ANDN,
        RISCV_ORN,
        RISCV_XNOR,
        RISCV_MAX,
        RISCV_MAXU,
        RISCV_MIN,
        RISCV_MINU,
        RISCV_ROL,
        RISCV_ROR
      } op;

    } zbb_rtype;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      enum extop_zbb {
        RISCV_SEXTB,
        RISCV_SEXTH,
        RISCV_ZEXTH
      } op;

    } zbb_extop;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_rev8;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_orcb;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_cpop;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_cpopw;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_clz;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_clzw;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_ctz;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_ctzw;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_clmul;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_clmulh;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_clmulr;

    struct  {
      uint8_t shamt /* bits : 6 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      enum biop_zbs {
        RISCV_BCLRI,
        RISCV_BEXTI,
        RISCV_BINVI,
        RISCV_BSETI
      } op;

    } zbs_iop;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      enum brop_zbs {
        RISCV_BCLR,
        RISCV_BEXT,
        RISCV_BINV,
        RISCV_BSET
      } op;

    } zbs_rtype;

    struct  {
      uint8_t uimm /* bits : 2 */;
      uint8_t rdc /* bits : 3 */;
      uint8_t rs1c /* bits : 3 */;
    } c_lbu;

    struct  {
      uint8_t uimm /* bits : 2 */;
      uint8_t rdc /* bits : 3 */;
      uint8_t rs1c /* bits : 3 */;
    } c_lhu;

    struct  {
      uint8_t uimm /* bits : 2 */;
      uint8_t rdc /* bits : 3 */;
      uint8_t rs1c /* bits : 3 */;
    } c_lh;

    struct  {
      uint8_t uimm /* bits : 2 */;
      uint8_t rs1c /* bits : 3 */;
      uint8_t rs2c /* bits : 3 */;
    } c_sb;

    struct  {
      uint8_t uimm /* bits : 2 */;
      uint8_t rs1c /* bits : 3 */;
      uint8_t rs2c /* bits : 3 */;
    } c_sh;

    uint8_t c_zext_b /* bits : 3 */;
    uint8_t c_sext_b /* bits : 3 */;
    uint8_t c_zext_h /* bits : 3 */;
    uint8_t c_sext_h /* bits : 3 */;
    uint8_t c_zext_w /* bits : 3 */;
    uint8_t c_not /* bits : 3 */;
    struct  {
      uint8_t rsdc /* bits : 3 */;
      uint8_t rs2c /* bits : 3 */;
    } c_mul;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      enum rounding_mode rm;
      uint8_t rd /* bits : 5 */;
      enum f_bin_rm_op_H {
        FADD_H,
        FSUB_H,
        FMUL_H,
        FDIV_H
      } op;

    } f_bin_rm_type_h;

    struct  {
      uint8_t rs3 /* bits : 5 */;
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      enum rounding_mode rm;
      uint8_t rd /* bits : 5 */;
      enum f_madd_op_H {
        FMADD_H,
        FMSUB_H,
        FNMSUB_H,
        FNMADD_H
      } op;

    } f_madd_type_h;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      enum f_bin_op_H {
        FSGNJ_H,
        FSGNJN_H,
        FSGNJX_H,
        FMIN_H,
        FMAX_H,
        FEQ_H,
        FLT_H,
        FLE_H
      } fle_h;

    } f_bin_type_h;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      enum rounding_mode rm;
      uint8_t rd /* bits : 5 */;
      enum f_un_rm_op_H {
        FSQRT_H,
        FCVT_W_H,
        FCVT_WU_H,
        FCVT_H_W,
        FCVT_H_WU,
        FCVT_H_S,
        FCVT_H_D,
        FCVT_S_H,
        FCVT_D_H,
        FCVT_L_H,
        FCVT_LU_H,
        FCVT_H_L,
        FCVT_H_LU
      } fcvt_h_lu;

    } f_un_rm_type_h;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      enum f_un_op_H {
        FCLASS_H,
        FMV_X_H,
        FMV_H_X
      } fmv_h_x;

    } f_un_type_h;

    struct  {
      uint8_t constantidx /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_fli_h;

    struct  {
      uint8_t constantidx /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_fli_s;

    struct  {
      uint8_t constantidx /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_fli_d;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_fminm_h;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_fmaxm_h;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_fminm_s;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_fmaxm_s;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_fminm_d;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_fmaxm_d;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      enum rounding_mode rm;
      uint8_t rd /* bits : 5 */;
    } riscv_fround_h;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      enum rounding_mode rm;
      uint8_t rd /* bits : 5 */;
    } riscv_froundnx_h;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      enum rounding_mode rm;
      uint8_t rd /* bits : 5 */;
    } riscv_fround_s;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      enum rounding_mode rm;
      uint8_t rd /* bits : 5 */;
    } riscv_froundnx_s;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      enum rounding_mode rm;
      uint8_t rd /* bits : 5 */;
    } riscv_fround_d;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      enum rounding_mode rm;
      uint8_t rd /* bits : 5 */;
    } riscv_froundnx_d;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_fmvh_x_d;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_fmvp_d_x;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_fleq_h;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_fltq_h;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_fleq_s;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_fltq_s;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_fleq_d;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_fltq_d;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_fcvtmod_w_d;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } sha256sig0;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } sha256sig1;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } sha256sum0;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } sha256sum1;

    struct  {
      uint8_t bs /* bits : 2 */;
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } aes32esmi;

    struct  {
      uint8_t bs /* bits : 2 */;
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } aes32esi;

    struct  {
      uint8_t bs /* bits : 2 */;
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } aes32dsmi;

    struct  {
      uint8_t bs /* bits : 2 */;
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } aes32dsi;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } sha512sig0l;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } sha512sig0h;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } sha512sig1l;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } sha512sig1h;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } sha512sum0r;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } sha512sum1r;

    struct  {
      uint8_t rnum /* bits : 4 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } aes64ks1i;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } aes64ks2;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } aes64im;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } aes64esm;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } aes64es;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } aes64dsm;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } aes64ds;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } sha512sig0;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } sha512sig1;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } sha512sum0;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } sha512sum1;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } sm3p0;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } sm3p1;

    struct  {
      uint8_t bs /* bits : 2 */;
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } sm4ed;

    struct  {
      uint8_t bs /* bits : 2 */;
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } sm4ks;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      enum brop_zbkb {
        RISCV_PACK,
        RISCV_PACKH
      } op;

    } zbkb_rtype;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } zbkb_packw;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_zip;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_unzip;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_brev8;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_xperm8;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } riscv_xperm4;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
      enum zicondop {
        RISCV_CZERO_EQZ,
        RISCV_CZERO_NEZ
      } riscv_czero_nez;

    } zicond_rtype;

    struct  {
      uint8_t ma /* bits : 1 */;
      uint8_t ta /* bits : 1 */;
      uint8_t sew /* bits : 3 */;
      uint8_t lmul /* bits : 3 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } vsetvli;

    struct  {
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } vsetvl;

    struct  {
      uint8_t ma /* bits : 1 */;
      uint8_t ta /* bits : 1 */;
      uint8_t sew /* bits : 3 */;
      uint8_t lmul /* bits : 3 */;
      uint8_t uimm /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } vsetivli;

    struct  {
      enum vvfunct6 {
        VV_VADD,
        VV_VSUB,
        VV_VMINU,
        VV_VMIN,
        VV_VMAXU,
        VV_VMAX,
        VV_VAND,
        VV_VOR,
        VV_VXOR,
        VV_VRGATHER,
        VV_VRGATHEREI16,
        VV_VSADDU,
        VV_VSADD,
        VV_VSSUBU,
        VV_VSSUB,
        VV_VSLL,
        VV_VSMUL,
        VV_VSRL,
        VV_VSRA,
        VV_VSSRL,
        VV_VSSRA
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vvtype;

    struct  {
      enum nvsfunct6 {
        NVS_VNSRL,
        NVS_VNSRA
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } nvstype;

    struct  {
      enum nvfunct6 {
        NV_VNCLIPU,
        NV_VNCLIP
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } nvtype;

    struct  {
      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } masktypev;

    struct  {
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } movetypev;

    struct  {
      enum vxfunct6 {
        VX_VADD,
        VX_VSUB,
        VX_VRSUB,
        VX_VMINU,
        VX_VMIN,
        VX_VMAXU,
        VX_VMAX,
        VX_VAND,
        VX_VOR,
        VX_VXOR,
        VX_VSADDU,
        VX_VSADD,
        VX_VSSUBU,
        VX_VSSUB,
        VX_VSLL,
        VX_VSMUL,
        VX_VSRL,
        VX_VSRA,
        VX_VSSRL,
        VX_VSSRA
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vxtype;

    struct  {
      enum nxsfunct6 {
        NXS_VNSRL,
        NXS_VNSRA
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } nxstype;

    struct  {
      enum nxfunct6 {
        NX_VNCLIPU,
        NX_VNCLIP
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } nxtype;

    struct  {
      enum vxsgfunct6 {
        VX_VSLIDEUP,
        VX_VSLIDEDOWN,
        VX_VRGATHER
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vxsg;

    struct  {
      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } masktypex;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } movetypex;

    struct  {
      enum vifunct6 {
        VI_VADD,
        VI_VRSUB,
        VI_VAND,
        VI_VOR,
        VI_VXOR,
        VI_VSADDU,
        VI_VSADD,
        VI_VSLL,
        VI_VSRL,
        VI_VSRA,
        VI_VSSRL,
        VI_VSSRA
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t simm /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vitype;

    struct  {
      enum nisfunct6 {
        NIS_VNSRL,
        NIS_VNSRA
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t simm /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } nistype;

    struct  {
      enum nifunct6 {
        NI_VNCLIPU,
        NI_VNCLIP
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t simm /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } nitype;

    struct  {
      enum visgfunct6 {
        VI_VSLIDEUP,
        VI_VSLIDEDOWN,
        VI_VRGATHER
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t simm /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } visg;

    struct  {
      uint8_t vs2 /* bits : 5 */;
      uint8_t simm /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } masktypei;

    struct  {
      uint8_t vd /* bits : 5 */;
      uint8_t simm /* bits : 5 */;
    } movetypei;

    struct  {
      uint8_t vs2 /* bits : 5 */;
      uint8_t simm /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vmvrtype;

    struct  {
      enum mvvfunct6 {
        MVV_VAADDU,
        MVV_VAADD,
        MVV_VASUBU,
        MVV_VASUB,
        MVV_VMUL,
        MVV_VMULH,
        MVV_VMULHU,
        MVV_VMULHSU,
        MVV_VDIVU,
        MVV_VDIV,
        MVV_VREMU,
        MVV_VREM
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } mvvtype;

    struct  {
      enum mvvmafunct6 {
        MVV_VMACC,
        MVV_VNMSAC,
        MVV_VMADD,
        MVV_VNMSUB
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } mvvmatype;

    struct  {
      enum wvvfunct6 {
        WVV_VADD,
        WVV_VSUB,
        WVV_VADDU,
        WVV_VSUBU,
        WVV_VWMUL,
        WVV_VWMULU,
        WVV_VWMULSU
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } wvvtype;

    struct  {
      enum wvfunct6 {
        WV_VADD,
        WV_VSUB,
        WV_VADDU,
        WV_VSUBU
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } wvtype;

    struct  {
      enum wmvvfunct6 {
        WMVV_VWMACCU,
        WMVV_VWMACC,
        WMVV_VWMACCSU
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } wmvvtype;

    struct  {
      enum vext2funct6 {
        VEXT2_ZVF2,
        VEXT2_SVF2
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vext2type;

    struct  {
      enum vext4funct6 {
        VEXT4_ZVF4,
        VEXT4_SVF4
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vext4type;

    struct  {
      enum vext8funct6 {
        VEXT8_ZVF8,
        VEXT8_SVF8
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vext8type;

    struct  {
      uint8_t vs2 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } vmvxs;

    struct  {
      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } mvvcompress;

    struct  {
      enum mvxfunct6 {
        MVX_VAADDU,
        MVX_VAADD,
        MVX_VASUBU,
        MVX_VASUB,
        MVX_VSLIDE1UP,
        MVX_VSLIDE1DOWN,
        MVX_VMUL,
        MVX_VMULH,
        MVX_VMULHU,
        MVX_VMULHSU,
        MVX_VDIVU,
        MVX_VDIV,
        MVX_VREMU,
        MVX_VREM
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } mvxtype;

    struct  {
      enum mvxmafunct6 {
        MVX_VMACC,
        MVX_VNMSAC,
        MVX_VMADD,
        MVX_VNMSUB
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } mvxmatype;

    struct  {
      enum wvxfunct6 {
        WVX_VADD,
        WVX_VSUB,
        WVX_VADDU,
        WVX_VSUBU,
        WVX_VWMUL,
        WVX_VWMULU,
        WVX_VWMULSU
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } wvxtype;

    struct  {
      enum wxfunct6 {
        WX_VADD,
        WX_VSUB,
        WX_VADDU,
        WX_VSUBU
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } wxtype;

    struct  {
      enum wmvxfunct6 {
        WMVX_VWMACCU,
        WMVX_VWMACC,
        WMVX_VWMACCUS,
        WMVX_VWMACCSU
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } wmvxtype;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vmvsx;

    struct  {
      enum fvvfunct6 {
        FVV_VADD,
        FVV_VSUB,
        FVV_VMIN,
        FVV_VMAX,
        FVV_VSGNJ,
        FVV_VSGNJN,
        FVV_VSGNJX,
        FVV_VDIV,
        FVV_VMUL
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } fvvtype;

    struct  {
      enum fvvmafunct6 {
        FVV_VMADD,
        FVV_VNMADD,
        FVV_VMSUB,
        FVV_VNMSUB,
        FVV_VMACC,
        FVV_VNMACC,
        FVV_VMSAC,
        FVV_VNMSAC
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } fvvmatype;

    struct  {
      enum fwvvfunct6 {
        FWVV_VADD,
        FWVV_VSUB,
        FWVV_VMUL
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } fwvvtype;

    struct  {
      enum fwvvmafunct6 {
        FWVV_VMACC,
        FWVV_VNMACC,
        FWVV_VMSAC,
        FWVV_VNMSAC
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } fwvvmatype;

    struct  {
      enum fwvfunct6 {
        FWV_VADD,
        FWV_VSUB
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } fwvtype;

    struct  {
      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      enum vfunary0 {
        FV_CVT_XU_F,
        FV_CVT_X_F,
        FV_CVT_F_XU,
        FV_CVT_F_X,
        FV_CVT_RTZ_XU_F,
        FV_CVT_RTZ_X_F
      } vfunary0;

      uint8_t vd /* bits : 5 */;
    } vfunary0;

    struct  {
      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      enum vfwunary0 {
        FWV_CVT_XU_F,
        FWV_CVT_X_F,
        FWV_CVT_F_XU,
        FWV_CVT_F_X,
        FWV_CVT_F_F,
        FWV_CVT_RTZ_XU_F,
        FWV_CVT_RTZ_X_F
      } vfwunary0;

      uint8_t vd /* bits : 5 */;
    } vfwunary0;

    struct  {
      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      enum vfnunary0 {
        FNV_CVT_XU_F,
        FNV_CVT_X_F,
        FNV_CVT_F_XU,
        FNV_CVT_F_X,
        FNV_CVT_F_F,
        FNV_CVT_ROD_F_F,
        FNV_CVT_RTZ_XU_F,
        FNV_CVT_RTZ_X_F
      } vfnunary0;

      uint8_t vd /* bits : 5 */;
    } vfnunary0;

    struct  {
      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      enum vfunary1 {
        FVV_VSQRT,
        FVV_VRSQRT7,
        FVV_VREC7,
        FVV_VCLASS
      } vfunary1;

      uint8_t vd /* bits : 5 */;
    } vfunary1;

    struct  {
      uint8_t vs2 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } vfmvfs;

    struct  {
      enum fvffunct6 {
        VF_VADD,
        VF_VSUB,
        VF_VMIN,
        VF_VMAX,
        VF_VSGNJ,
        VF_VSGNJN,
        VF_VSGNJX,
        VF_VDIV,
        VF_VRDIV,
        VF_VMUL,
        VF_VRSUB,
        VF_VSLIDE1UP,
        VF_VSLIDE1DOWN
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } fvftype;

    struct  {
      enum fvfmafunct6 {
        VF_VMADD,
        VF_VNMADD,
        VF_VMSUB,
        VF_VNMSUB,
        VF_VMACC,
        VF_VNMACC,
        VF_VMSAC,
        VF_VNMSAC
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } fvfmatype;

    struct  {
      enum fwvffunct6 {
        FWVF_VADD,
        FWVF_VSUB,
        FWVF_VMUL
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } fwvftype;

    struct  {
      enum fwvfmafunct6 {
        FWVF_VMACC,
        FWVF_VNMACC,
        FWVF_VMSAC,
        FWVF_VNMSAC
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } fwvfmatype;

    struct  {
      enum fwffunct6 {
        FWF_VADD,
        FWF_VSUB
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } fwftype;

    struct  {
      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vfmerge;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vfmv;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vfmvsf;

    struct  {
      uint8_t nf /* bits : 3 */;
      uint8_t vm /* bits : 1 */;
      uint8_t rs1 /* bits : 5 */;
      enum vlewidth {
        VLE8,
        VLE16,
        VLE32,
        VLE64
      } width;

      uint8_t vd /* bits : 5 */;
    } vlsegtype;

    struct  {
      uint8_t nf /* bits : 3 */;
      uint8_t vm /* bits : 1 */;
      uint8_t rs1 /* bits : 5 */;
      enum vlewidth width;
      uint8_t vd /* bits : 5 */;
    } vlsegfftype;

    struct  {
      uint8_t nf /* bits : 3 */;
      uint8_t vm /* bits : 1 */;
      uint8_t rs1 /* bits : 5 */;
      enum vlewidth width;
      uint8_t vs3 /* bits : 5 */;
    } vssegtype;

    struct  {
      uint8_t nf /* bits : 3 */;
      uint8_t vm /* bits : 1 */;
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      enum vlewidth width;
      uint8_t vd /* bits : 5 */;
    } vlssegtype;

    struct  {
      uint8_t nf /* bits : 3 */;
      uint8_t vm /* bits : 1 */;
      uint8_t rs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      enum vlewidth width;
      uint8_t vs3 /* bits : 5 */;
    } vsssegtype;

    struct  {
      uint8_t nf /* bits : 3 */;
      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      enum vlewidth width;
      uint8_t vd /* bits : 5 */;
    } vluxsegtype;

    struct  {
      uint8_t nf /* bits : 3 */;
      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      enum vlewidth width;
      uint8_t vd /* bits : 5 */;
    } vloxsegtype;

    struct  {
      uint8_t nf /* bits : 3 */;
      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      enum vlewidth width;
      uint8_t vs3 /* bits : 5 */;
    } vsuxsegtype;

    struct  {
      uint8_t nf /* bits : 3 */;
      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      enum vlewidth width;
      uint8_t vs3 /* bits : 5 */;
    } vsoxsegtype;

    struct  {
      uint8_t nf /* bits : 3 */;
      uint8_t rs1 /* bits : 5 */;
      enum vlewidth width;
      uint8_t vd /* bits : 5 */;
    } vlretype;

    struct  {
      uint8_t nf /* bits : 3 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t vs3 /* bits : 5 */;
    } vsretype;

    struct  {
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd_or_vs3 /* bits : 5 */;
      enum vmlsop {
        VLM,
        VSM
      } op;

    } vmtype;

    struct  {
      enum mmfunct6 {
        MM_VMAND,
        MM_VMNAND,
        MM_VMANDN,
        MM_VMXOR,
        MM_VMOR,
        MM_VMNOR,
        MM_VMORN,
        MM_VMXNOR
      } funct6;

      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } mmtype;

    struct  {
      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } vcpop_m;

    struct  {
      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t rd /* bits : 5 */;
    } vfirst_m;

    struct  {
      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vmsbf_m;

    struct  {
      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vmsif_m;

    struct  {
      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vmsof_m;

    struct  {
      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } viota_m;

    struct  {
      uint8_t vm /* bits : 1 */;
      uint8_t vd /* bits : 5 */;
    } vid_v;

    struct  {
      enum vvmfunct6 {
        VVM_VMADC,
        VVM_VMSBC
      } funct6;

      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vvmtype;

    struct  {
      enum vvmcfunct6 {
        VVMC_VMADC,
        VVMC_VMSBC
      } funct6;

      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vvmctype;

    struct  {
      enum vvmsfunct6 {
        VVMS_VADC,
        VVMS_VSBC
      } funct6;

      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vvmstype;

    struct  {
      enum vvcmpfunct6 {
        VVCMP_VMSEQ,
        VVCMP_VMSNE,
        VVCMP_VMSLTU,
        VVCMP_VMSLT,
        VVCMP_VMSLEU,
        VVCMP_VMSLE
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vvcmptype;

    struct  {
      enum vxmfunct6 {
        VXM_VMADC,
        VXM_VMSBC
      } funct6;

      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vxmtype;

    struct  {
      enum vxmcfunct6 {
        VXMC_VMADC,
        VXMC_VMSBC
      } funct6;

      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vxmctype;

    struct  {
      enum vxmsfunct6 {
        VXMS_VADC,
        VXMS_VSBC
      } funct6;

      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vxmstype;

    struct  {
      enum vxcmpfunct6 {
        VXCMP_VMSEQ,
        VXCMP_VMSNE,
        VXCMP_VMSLTU,
        VXCMP_VMSLT,
        VXCMP_VMSLEU,
        VXCMP_VMSLE,
        VXCMP_VMSGTU,
        VXCMP_VMSGT
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vxcmptype;

    struct  {
      enum vimfunct6 {
        VIM_VMADC
      } funct6;

      uint8_t vs2 /* bits : 5 */;
      uint8_t simm /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vimtype;

    struct  {
      enum vimcfunct6 {
        VIMC_VMADC
      } funct6;

      uint8_t vs2 /* bits : 5 */;
      uint8_t simm /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vimctype;

    struct  {
      enum vimsfunct6 {
        VIMS_VADC
      } funct6;

      uint8_t vs2 /* bits : 5 */;
      uint8_t simm /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vimstype;

    struct  {
      enum vicmpfunct6 {
        VICMP_VMSEQ,
        VICMP_VMSNE,
        VICMP_VMSLEU,
        VICMP_VMSLE,
        VICMP_VMSGTU,
        VICMP_VMSGT
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t simm /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } vicmptype;

    struct  {
      enum fvvmfunct6 {
        FVVM_VMFEQ,
        FVVM_VMFLE,
        FVVM_VMFLT,
        FVVM_VMFNE
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } fvvmtype;

    struct  {
      enum fvfmfunct6 {
        VFM_VMFEQ,
        VFM_VMFLE,
        VFM_VMFLT,
        VFM_VMFNE,
        VFM_VMFGT,
        VFM_VMFGE
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t rs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } fvfmtype;

    struct  {
      enum rivvfunct6 {
        IVV_VWREDSUMU,
        IVV_VWREDSUM
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } rivvtype;

    struct  {
      enum rmvvfunct6 {
        MVV_VREDSUM,
        MVV_VREDAND,
        MVV_VREDOR,
        MVV_VREDXOR,
        MVV_VREDMINU,
        MVV_VREDMIN,
        MVV_VREDMAXU,
        MVV_VREDMAX
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } rmvvtype;

    struct  {
      enum rfvvfunct6 {
        FVV_VFREDOSUM,
        FVV_VFREDUSUM,
        FVV_VFREDMAX,
        FVV_VFREDMIN,
        FVV_VFWREDOSUM,
        FVV_VFWREDUSUM
      } funct6;

      uint8_t vm /* bits : 1 */;
      uint8_t vs2 /* bits : 5 */;
      uint8_t vs1 /* bits : 5 */;
      uint8_t vd /* bits : 5 */;
    } rfvvtype;

    struct  {
      enum cbop_zicbom {
        CBO_CLEAN,
        CBO_FLUSH,
        CBO_INVAL
      } cbop;

      uint8_t rs1 /* bits : 5 */;
    } riscv_zicbom;

    uint8_t riscv_zicboz /* bits : 5 */;
  } ast_node;

} ;
