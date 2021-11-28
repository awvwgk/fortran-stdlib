set(dir "stdlib_linalg")

list(
  APPEND fppFiles
  "${dir}/blas1.fypp"
  "${dir}/blas2.fypp"
  "${dir}/blas3.fypp"
)
set(fppFiles "${fppFiles}" PARENT_SCOPE)
