      module chanel_C 
      USE vast_kind_param, ONLY:  double 
!...Created by Pacific-Sierra Research 77to90  4.4G  10:36:01  03/09/06  
  !          Channel numbers are assigned as follows:
   !
   !    File              Channel  Name
   !
   !    Input                2   <Filename>.dat         FORMATTED
   !    Scratch              3   <Filename>.scr         UNFORMATTED
   !    End                  4   <Filename>.end         FORMATTED
   !    Input (During run)  25   <Filename>.temp        FORMATTED
   !    Output              26   <Filename>.out         FORMATTED
   !    Setup               27   SETUP / user specified FORMATTED
   !    Error (moldat debug) 8   <Filename>.err         FORMATTED
   !    Restart              9   <Filename>.res         UNFORMATTED
   !    Density             10   <Filename>.den         UNFORMATTED
   !    Log                 11   <Filename>.log         FORMATTED
   !    Archive             12   <Filename>.arc         FORMATTED
   !    Graphics            13   <Filename>.gpt         UNFORMATTED
   !    EXTERNAL params     14   Defined by EXTERNAL    FORMATTED
   !    ESP restart         15   <Filename>.esr         UNFORMATTED
   !    SOL map in MEP      17   <Filename>.sol         FORMATTED
   !    Brillouin Zone      18   <Filename>.brz         UNFORMATTED
   !    PDB output          19   <Filename>.pdb         FORMATTED
   !    Grid map            20   <Filename>.ump         FORMATTED
   !    Electrostatic map   21   <Filename>.esp         FORMATTED
   !    MEP map             22   <Filename>.mep         FORMATTED
   !    DAF (polarization)  29   <Filename>.pol         FORMATTED
   !    COSMO output        30   <Filename>.cos         FORMATTED
   !    WVB output          52   <Filename>.wvb         FORMATTED
   !    WES output          53   <Filename>.wes         FORMATTED
   !    TMP output          54   <Filename>.tmp         FORMATTED

    integer ::       iw = 26
    integer :: &
     & ifiles_1 = 1, &
     & input = 2, &
     & iscr = 3, &
     & irot = 2, &
     & iend = 4, &
     & ir = 25, &
     & iferr = 8, &
     & ires = 9, &
     & iden = 10, &
     & ilog = 31, &
     & iarc = 12, &
     & igpt = 13, &
     & iext = 14, &
     & iesr = 15, &
     & isyb = 16, &
     & isol = 17, &
     & ibrz = 18, &
     & ipdb = 19, &
     & iump = 20, &
     & iesp = 21, &
     & imep = 22, &
     & param_out = 24, &
     & isetup = 27, &
     & lbfgs_it = 28, &
     & idaf = 29, &
     & is = 0

          integer, parameter :: filename_len = 240

    character(len=filename_len) :: &
     & job_fn, &
     & output_fn, &
     & restart_fn, &
     & density_fn, &
     & log_fn, &
     & end_fn, &
     & ext_fn, &
     & archive_fn

      integer, dimension(145) :: ifilen, ioda 
      integer :: irecln = 1023, irecst 
      end module chanel_C 
