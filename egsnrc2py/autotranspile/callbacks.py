
# EMPTY CALLBACKS ----
add_work_em_field = None
call_howfar_in_electr = None
call_user_electron = None
compute_range = None
de_fluctuation = None
electron_track_end = None
em_field_ss = None
emfield_initiate_set_tustep = None
emfieldinvacuum = None
evaluate_bhabha_fraction = None
evaluate_ebrem_fraction = None
evaluate_pbrem_fraction = None
evaluate_sig0 = None
implicit_none = None
integer = None
logical = None
particle_selection_annih = None
particle_selection_annihrest = None
particle_selection_bhabha = None
particle_selection_brems = None
particle_selection_moller = None
range_discard = None
real = None
scale_sig0 = None
select_electron_mfp = None
set_angles_em_field = None
set_rhof = None
set_tustep_em_field = None
set_tvstep_em_field = None
set_ustep_em_field = None
start_new_particle = None
update_demfp = None
user_controls_tstep_recursion = None
user_range_discard = None
vacuum_add_work_em_field = None


# CALLBACKS ---- 
def calculate_tstep_from_demfp([]):
;
      IF( compute_tstep ) [
        total_de = demfp/sig; fedep = total_de;
        ekef  = eke - fedep;
        IF( ekef <= E_array(1,medium) ) [ tstep = vacdst; ]
        ELSE
        [
          elkef = Log(ekef);
          $SET INTERVAL elkef,eke;
          IF( lelkef = lelke )
          [       " initial and final energy are in the same interpolation bin "
              $COMPUTE_DRANGE(eke,ekef,lelke,elke,elkef,tstep);
          ]
          ELSE
          [   " initial and final energy are in different interpolation bins, "
              " calc range from ekef to E(lelkef+1) and from E(lelke) to eke  "
              " and add the pre-calculated range from E(lelkef+1) to E(lelke) "
              ekei = E_array(lelke,medium);
              elkei = (lelke - eke0(medium))/eke1(medium);
              $COMPUTE_DRANGE(eke,ekei,lelke,elke,elkei,tuss);
              ekei = E_array(lelkef+1,medium);
              elkei = (lelkef + 1 - eke0(medium))/eke1(medium);
              $COMPUTE_DRANGE(ekei,ekef,lelkef,elkei,elkef,tstep);
              tstep=tstep+tuss+
                      range_ep(qel,lelke,medium)-range_ep(qel,lelkef+1,medium);
          ]
        ]
        total_tstep = tstep;
        compute_tstep = .false.;
      ]
      tstep = total_tstep/rhof;  " non-default density scaling "
def check_negative_ustep([]):
;
        IF(ustep <= 0) [
            "Negative ustep---probable truncation problem at a"
            "boundary, which means we are not in the region we think"
            "we are in.  The default macro assumes that user has set"
            "irnew to the region we are really most likely to be"
            "in.  A message is written out whenever ustep is less than -1.e-4"
            IF(ustep < -1e-4) [
                ierust = ierust + 1;
                OUTPUT ierust,ustep,dedx,e(np)-prm,
                       ir(np),irnew,irold,x(np),y(np),z(np);
                (i4,' Negative ustep = ',e12.5,' dedx=',F8.4,' ke=',F8.4,
                 ' ir,irnew,irold =',3i4,' x,y,z =',4e10.3);
                IF(ierust > 1000) [
                    OUTPUT;(////' Called exit---too many ustep errors'///);
                    $CALL_EXIT(1);
                ]
            ]
            ustep = 0;
        ]
def evaluate_sigf([]):
;
      IF(lelec < 0)
      [
           sigf = esig1[Lelke,MEDIUM]*elke+ esig0[Lelke,MEDIUM]  # EVALUATE sigf USING esig(elke)
           dedx0 = ededx1[Lelke,MEDIUM]*elke+ ededx0[Lelke,MEDIUM]  # EVALUATE dedx0 USING ededx(elke)
          sigf = sigf/dedx0;
      ]
      ELSE
      [
           sigf = psig1[Lelke,MEDIUM]*elke+ psig0[Lelke,MEDIUM]  # EVALUATE sigf USING psig(elke)
           dedx0 = pdedx1[Lelke,MEDIUM]*elke+ pdedx0[Lelke,MEDIUM]  # EVALUATE dedx0 USING pdedx(elke)
          sigf = sigf/dedx0;
      ]
def set_tvstep([]):

        ;IF ( vstep < ustep0 )
        [
          ekems = eke - 0.5*tustep*vstep/ustep0*dedx;
             "This estimates the energy loss to the boundary."
             "tustep was the intended curved path-length,"
             "ustep0 is the average transport distance in the initial direction"
             "       resulting from tustep"
             "vstep = ustep is the reduced average transport distance in the "
             "              initial direction due to boundary crossing"
          $CALCULATE_XI(vstep);
          IF ( xi < 0.1 )
          [
            tvstep = vstep*(1 + xi*(0.5 + xi*0.333333));
          ]
          ELSE
          [
    
            IF ( xi < 0.999999 )
            [
               tvstep = -vstep*Log(1 - xi)/xi;
            ]
            ELSE
            [
               "This is an error condition because the average transition "
               "in the initial direction of motion is always smaller than 1/Q1"
               $egs_info(*,' Stoped in SET-TVSTEP because xi > 1! ');
               $egs_info(*,' Medium: ',medium);
               $egs_info(*,' Initial energy: ',eke);
               $egs_info(*,' Average step energy: ',ekems);
               $egs_info(*,' tustep: ',tustep);
               $egs_info(*,' ustep0: ',ustep0);
               $egs_info(*,' vstep:  ',vstep);
               $egs_info(*,' ==> xi = ',xi);
               $egs_fatal(*,'This is a fatal error condition');
            ]
          ]
        ]
        ELSE
        [
          tvstep = tustep;
        ]
def set_ustep([]):

      ekems = eke - 0.5*tustep*dedx; "Use mid-point energy to calculate"
                                      "energy dependent quantities"
      $CALCULATE_XI(tustep);
      IF ( xi < 0.1 )
        [
          ustep = tustep*(1 - xi*(0.5 - xi*0.166667));
        ]
        ELSE
        [
          ustep = tustep*(1 - Exp(-xi))/xi;
        ]

