! SPDX-Identifier: MIT

!> Interface module for BLAS routines
module stdlib_linalg_blas
  use stdlib_linalg_blas1, only : &
    & blas_axpy, blas_copy, blas_dot, blas_dotc, blas_dotu, blas_dsdot, blas_sdsdot, &
    & blas_rotg, blas_rotm, blas_rotmg, blas_scal, blas_rot, blas_swap, blas_abs1, &
    & blas_asum, blas_nrm2, blas_iamax
  use stdlib_linalg_blas2, only : &
    & blas_gbmv, blas_gemv, blas_ger, blas_gerc, blas_geru, blas_sbmv, blas_spmv, &
    & blas_hbmv, blas_hemv, blas_spr2, blas_spr, blas_syr2, blas_syr, blas_her2, blas_her, &
    & blas_symv, blas_hpmv, blas_hpr2, blas_hpr, blas_tbmv, blas_tbsv, blas_tpmv, blas_tpsv, &
    & blas_trmv, blas_trsv
  use stdlib_linalg_blas3, only : &
    & blas_gemm, blas_hemm, blas_her2k, blas_herk, blas_symm, blas_syr2k, blas_syrk, &
    & blas_trsm, blas_trmm
  implicit none
  private

  public :: blas_axpy, blas_copy, blas_dot, blas_dotc, blas_dotu, blas_dsdot, blas_sdsdot, &
    & blas_rotg, blas_rotm, blas_rotmg, blas_scal, blas_rot, blas_swap, blas_abs1, &
    & blas_asum, blas_nrm2, blas_iamax

  public :: blas_gbmv, blas_gemv, blas_ger, blas_gerc, blas_geru, blas_sbmv, blas_spmv, &
    & blas_hbmv, blas_hemv, blas_spr2, blas_spr, blas_syr2, blas_syr, blas_her2, blas_her, &
    & blas_symv, blas_hpmv, blas_hpr2, blas_hpr, blas_tbmv, blas_tbsv, blas_tpmv, blas_tpsv, &
    & blas_trmv, blas_trsv

  public :: blas_gemm, blas_hemm, blas_her2k, blas_herk, blas_symm, blas_syr2k, blas_syrk, &
    & blas_trsm, blas_trmm

end module stdlib_linalg_blas
