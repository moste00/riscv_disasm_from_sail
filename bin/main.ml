open Libsail
open Type_check
open Ast
open Riscv_disasm_from_sail

let sailpath = "/home/mostafa/.opam/default/share/sail/"

let files =
  [
    "model/prelude.sail";
    "model/riscv_xlen64.sail";
    "model/riscv_flen_D.sail";
    "model/riscv_vlen.sail";
    "model/prelude_mem_metadata.sail";
    "model/prelude_mem.sail";
    "model/riscv_types_common.sail";
    "model/riscv_types_ext.sail";
    "model/riscv_types.sail";
    "model/riscv_vmem_types.sail";
    "model/riscv_reg_type.sail";
    "model/riscv_freg_type.sail";
    "model/riscv_regs.sail";
    "model/riscv_pc_access.sail";
    "model/riscv_sys_regs.sail";
    "model/riscv_pmp_regs.sail";
    "model/riscv_pmp_control.sail";
    "model/riscv_ext_regs.sail";
    "model/riscv_addr_checks_common.sail";
    "model/riscv_addr_checks.sail";
    "model/riscv_misa_ext.sail";
    "model/riscv_vreg_type.sail";
    "model/riscv_vext_regs.sail";
    "model/riscv_csr_map.sail";
    "model/riscv_vext_control.sail";
    "model/riscv_next_regs.sail";
    "model/riscv_sys_exceptions.sail";
    "model/riscv_sync_exception.sail";
    "model/riscv_next_control.sail";
    "model/riscv_softfloat_interface.sail";
    "model/riscv_fdext_regs.sail";
    "model/riscv_fdext_control.sail";
    "model/riscv_csr_ext.sail";
    "model/riscv_sys_control.sail";
    "model/riscv_platform.sail";
    "model/riscv_mem.sail";
    "model/riscv_vmem_common.sail";
    "model/riscv_vmem_pte.sail";
    "model/riscv_vmem_ptw.sail";
    "model/riscv_vmem_tlb.sail";
    "model/riscv_vmem.sail";
    "model/riscv_types_kext.sail";
    "model/riscv_insts_begin.sail";
    (*"model/riscv_insts_base.sail";
      "model/riscv_insts_aext.sail";
      "model/riscv_insts_cext.sail" ;
      "model/riscv_insts_mext.sail" ;
      "model/riscv_insts_zicsr.sail" ;
      "model/riscv_insts_next.sail" ;
      "model/riscv_insts_hints.sail" ;
      "model/riscv_insts_fext.sail" ;
      "model/riscv_insts_cfext.sail";
      "model/riscv_insts_dext.sail";
      "model/riscv_insts_cdext.sail";
      "model/riscv_insts_svinval.sail";
      "model/riscv_insts_zba.sail";
      "model/riscv_insts_zbb.sail";
      "model/riscv_insts_zbc.sail";
      "model/riscv_insts_zbs.sail";
      "model/riscv_insts_zcb.sail";
      "model/riscv_insts_zfh.sail";
      "model/riscv_insts_zfa.sail";
      "model/riscv_insts_zkn.sail";
      "model/riscv_insts_zks.sail";
      "model/riscv_insts_zbkb.sail";
      "model/riscv_insts_zbkx.sail";
      "model/riscv_insts_zicond.sail";
      "model/riscv_insts_vext_utils.sail";
      "model/riscv_insts_vext_vset.sail";
      "model/riscv_insts_vext_arith.sail";
      "model/riscv_insts_vext_fp.sail";
      "model/riscv_insts_vext_mem.sail";
      "model/riscv_insts_vext_mask.sail";
      "model/riscv_insts_vext_vm.sail";
      "model/riscv_insts_vext_red.sail";
      "model/riscv_jalr_seq.sail";
      "model/riscv_insts_end.sail";
      "model/riscv_step_common.sail";
      "model/riscv_step_ext.sail";
      "model/riscv_decode_ext.sail";
      "model/riscv_fetch.sail";
      "model/riscv_step.sail";
      "model/main.sail"*)
  ]
let filepaths = List.map (function f -> "../sail-riscv/" ^ f) files

let initial_typeenv = Type_check.initial_env

let dummyoptions =
  [
    ("-lem_extern_type", Arg.String (fun _ -> ()), "");
    ("-coq_extern_type", Arg.String (fun _ -> ()), "");
  ]

let ast, types, side_effects =
  Frontend.load_files sailpath dummyoptions initial_typeenv filepaths

let type_reg = Gen_disasm.collect_types ast
let x = Gen_disasm.gen_clike_ast "ast" type_reg
