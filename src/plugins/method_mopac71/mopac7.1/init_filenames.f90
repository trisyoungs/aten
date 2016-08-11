 subroutine init_filenames
    use molkst_C, only: jobnam
    use chanel_C, only : output_fn, restart_fn, &
     & density_fn, log_fn, end_fn, archive_fn
    integer :: i, text_length
    text_length = len_trim (jobnam)
    do i = text_length, 1, -1
     if(jobnam(i:i) /= "?") exit
     jobnam(i:i) = " "
    end do


    output_fn     = jobnam(1:text_length) // ".out"
    restart_fn    = jobnam(1:text_length) // ".res"
    density_fn    = jobnam(1:text_length) // ".den"
    log_fn        = jobnam(1:text_length) // ".log"
    end_fn        = jobnam(1:text_length) // ".end"
    archive_fn    = jobnam(1:text_length) // ".arc"
  end subroutine init_filenames
