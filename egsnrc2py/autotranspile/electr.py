# !COMMENTS
# !INDENT C5
# !INDENT M4
# !INDENT F2


# ******************************************************************
#                                NATIONAL RESEARCH COUNCIL OF CANADA
def ELECTR(IRCODE):
# ******************************************************************
#    This subroutine has been almost completely recoded to include  
#    the EGSnrc enhancements.                                       
#                                                                   
#    Version 1.0   Iwan Kawrakow       Complete recoding            
#    Version 1.1   Iwan Kawrakow       Corrected implementation of  
#                                      fictitious method (important 
#                                      for low energy transport     
# ******************************************************************

implicit none

;integer*4 IRCODE

# $ comin_electr # default replacement produces the following:
               # COMIN/DEBUG,BOUNDS,EGS-VARIANCE-REDUCTION, ELECIN,EPCONT,
                        # ET-Control,MEDIA,MISC,STACK,THRESH,UPHIIN,
                        # UPHIOT,USEFUL,USER,RANDOM/
# ;comin/eii-data/
# ;comin/em/
;real*8 lambda_max, sigratio, u_tmp, v_tmp, w_tmp
LOGICAL random_tustep

# $ define_local_variables_electr
/******* trying to save evaluation of range.
;logical  do_range
;real*8     the_range
*/

data ierust/0/ # To count negative ustep's

save ierust

# --- Inline replace: $ CALL_USER_ELECTRON -----
if call_user_electron:
    call_user_electron()
# End inline replace: $ CALL_USER_ELECTRON ----

ircode = 1 # Set up normal return-which means there is a photon
            # with less available energy than the lowest energy electron,
            # so return to shower so it can call photon to follow it.
            # (For efficiency's sake, we like to stay in this routine
            #  as long as there are electrons to process. That's why this
            #  apparently convoluted scheme of STACK contro is effected.)

irold = ir[np] # Initialize previous region
                   # (ir() is an integer that is attached to the particle's
                   #  phase space. It contains the region
                   #  number that the current particle is in.
                   #  Np is the stack pointer, it points to where on the
                   #  stack the current particle is.)
irl    = irold # region number in local variable


# --- Inline replace: $ start_new_particle; -----
if start_new_particle:
    start_new_particle()
else:
    medium = med(irl) 
# End inline replace: $ start_new_particle; ----
#  Default replacement for the above is medium = med(irl) 
#  This is made a macro so that it can be replaced with a call to a 
#  user provided function start_new_particle(); for the C/C++ interface 

while True:  # :NEWELECTRON: LOOP

    # Go once through this loop for each 'new' electron whose charge and
    # energy has not been checked

    lelec = iq[np] # Save charge in local variable
                    # (iq = -1 for electrons, 0 for photons and 1 for positrons)
    qel   = (1+lelec)/2 #  = 0 for electrons, = 1 for positrons 
    peie  = e[np] # precise energy of incident electron (double precision)
    eie   = peie # energy incident electron (conversion to single)

    if eie <= ecut(irl):

        go to :ECUT-DISCARD:
        # (Ecut is the lower transport threshold.)

    # medium = med(irl) # (This renders the above assignment redundant!)
    # The above assignment is unnecessary, IK, June 2003

    if wt[np] == 0.0:

        go to :USER-ELECTRON-DISCARD: # added May 01

    while True:  # :TSTEP: LOOP

        # Go through this loop each time we recompute distance to an interaction
        /******* trying to save evaluation of range.
        do_range = True # compute the range in $ COMPUTE-RANGE below 
        ********/
        compute_tstep = True # MFP resampled => calculate distance to the
                                # interaction in the USTEP loop
        eke = eie - rm # moved here so that kinetic energy will be known
                        # to user even for a vacuum step, IK January 2000
        if medium != 0:

            # Not vacuum. Must sample to see how far to next interaction.

            # --- Inline replace: $ SELECT_ELECTRON_MFP; -----
            if select_electron_mfp:
                select_electron_mfp()
            else:
    
                # --- Inline replace: $ RANDOMSET RNNE1; -----
                if randomset:
                    RNNE1 = randomset()
                else:
                    
                      if  rng_seed > 24 :

                          call ranlux(rng_array); rng_seed = 1

                       RNNE1 = rng_array(rng_seed)
                      rng_seed = rng_seed + 1
                    
                # End inline replace: $ RANDOMSET RNNE1; ---- IF(RNNE1.EQ.0.0) [RNNE1=1.E-30;]
                 DEMFP=MAX(-LOG(RNNE1),EPSEMFP)
            # End inline replace: $ SELECT_ELECTRON_MFP; ----
                #  Default FOR $ SELECT-ELECTRON-MFP; is: $ RANDOMSET rnne1
                #                                        demfp = -log(rnne1)
                # ($ RANDOMSET is a macro'ed random number generator) 
                # (demfp = differential electron mean free path)

            elke = log(eke)
            # (eke = kinetic energy, rm = rest mass, all in units of MeV)
            # Unhandled macro '$ SET INTERVAL elke,eke;' # Prepare to approximate cross section

            # --- Inline replace: $ EVALUATE_SIG0; -----
            if evaluate_sig0:
                evaluate_sig0()
            else:
                
                   if  sig_ismonotone(qel,medium) :

                       # --- Inline replace: $ EVALUATE_SIGF; -----
                       if evaluate_sigf:
                           evaluate_sigf()
                       else:
                           
                             if lelec < 0:

                                  sigf = esig1[Lelke,MEDIUM]*elke+ esig0[Lelke,MEDIUM]  # EVALUATE sigf USING esig(elke)
                                  dedx0 = ededx1[Lelke,MEDIUM]*elke+ ededx0[Lelke,MEDIUM]  # EVALUATE dedx0 USING ededx(elke)
                                 sigf = sigf/dedx0
                             else:

                                  sigf = psig1[Lelke,MEDIUM]*elke+ psig0[Lelke,MEDIUM]  # EVALUATE sigf USING psig(elke)
                                  dedx0 = pdedx1[Lelke,MEDIUM]*elke+ pdedx0[Lelke,MEDIUM]  # EVALUATE dedx0 USING pdedx(elke)
                                 sigf = sigf/dedx0

                       # End inline replace: $ EVALUATE_SIGF; ---- sig0 = sigf
                   else:
                       if  lelec < 0 :
                           sig0 = esig_e(medium)
                       else:
                           sig0 = psig_e(medium)

            # End inline replace: $ EVALUATE_SIG0; ----
               # The fix up of the fictitious method uses cross section per
               # energy loss. Therefore, demfp/sig is sub-threshold energy loss
               # until the next discrete interaction occures (see below)
               # As this quantity is a single constant for a material,
               # $ SET INTERVAL is not necessary at this point. However, to not 
               # completely alter the logic of the TSTEP and USTEP loops,
               # this is left for now

        ] # end non-vacuum test

        while True:  # :USTEP: LOOP

            # Here for each check with user geometry.
            # Compute size of maximum acceptable step, which is limited
            # by multiple scattering or other approximations.
            if medium == 0:

                    # vacuum

                    tstep = vacdst; ustep = tstep; tustep = ustep
                    callhowfar = True # Always call HOWFAR for vacuum steps!

                    # (Important definitions:
                    #  tstep  = total pathlength to the next discrete interaction
                    #  vacdst = infinity (actually 10^8)
                    #  tustep = total pathlength of the electron step
                    #  ustep  = projected transport distance in the
                    #           direction of motion at the start of the step
                    #  Note that tustep and ustep are modified below.
                    #  The above provide defaults.)

                    #  EM field step size restriction in vacuum

                    ustep = tustep
            else:

                # non-vacuum
RHOF=RHOR(IRL)/RHO(MEDIUM) # density ratio scaling template
                              # EGS allows the density to vary
                              # continuously (user option)

                # --- Inline replace: $ SCALE_SIG0; -----
                if scale_sig0:
                    scale_sig0()
                else:
    
                    sig = sig0
                # End inline replace: $ SCALE_SIG0; ----
                if sig <= 0:

                    # This can happen if the threshold for brems,
                    # (ap + rm), is greater than ae.  Moller threshold is
                    # 2*ae - rm. If sig is zero, we are below the
                    # thresholds for both bremsstrahlung and Moller.
                    # In this case we will just lose energy by
                    # ionization loss until we go below cut-off. Do not
                    # assume range is available, so just ask for step
                    # same as vacuum.  Electron transport will reduce
                    # into little steps.
                    # (Note: ae is the lower threshold for creation of a
                    #        secondary Moller electron, ap is the lower
                    #        threshold for creation of a brem.)
                    tstep = vacdst
                    sig0 = 1.E-15
                else:

                    # --- Inline replace: $ CALCULATE_TSTEP_FROM_DEMFP; -----
                    if calculate_tstep_from_demfp:
                        calculate_tstep_from_demfp()
                    else:
                        
                          if  compute_tstep :

                            total_de = demfp/sig; fedep = total_de
                            ekef  = eke - fedep
                            if  ekef <= E_array(1,medium) :
                                 tstep = vacdst 
                            else:

                              elkef = Log(ekef)
                              # Unhandled macro '$ SET INTERVAL elkef,eke;'
                              if  lelkef == lelke :
              
                              [       #  initial and final energy are in the same interpolation bin 
                                  # --- Inline replace: $ COMPUTE_DRANGE(eke,ekef,lelke,elke,elkef,tstep); -----
                                  if compute_drange:
                                      tstep = compute_drange(eke1, eke2, lelke1, elke1, elke2)
                                  else:
        
                                      fedep = 1 - ekef/eke
                                      elktmp = 0.5*(elke+elkef+0.25*fedep*fedep*(1+fedep*(1+0.875*fedep)))
                                               #  the above evaluates the logarithm of the midpoint energy
                                      lelktmp = lelke
                                      if lelec < 0:

                                           dedxmid = ededx1[Lelktmp,MEDIUM]*elktmp+ ededx0[Lelktmp,MEDIUM]  # EVALUATE dedxmid USING ededx(elktmp)
                                          dedxmid = 1/dedxmid
                                          aux = ededx1(lelktmp,medium)*dedxmid
                                          # aux = ededx1(lelktmp,medium)/dedxmid
                                      else:
                                           dedxmid = pdedx1[Lelktmp,MEDIUM]*elktmp+ pdedx0[Lelktmp,MEDIUM]  # EVALUATE dedxmid USING pdedx(elktmp)
                                          dedxmid = 1/dedxmid
                                          aux = pdedx1(lelktmp,medium)*dedxmid
                                          # aux = pdedx1(lelktmp,medium)/dedxmid

                                      aux = aux*(1+2*aux)*(fedep/(2-fedep))**2/6
                                      # tstep = fedep*eke/dedxmid*(1+aux)
                                      tstep = fedep*eke*dedxmid*(1+aux)
                                  # End inline replace: $ COMPUTE_DRANGE(eke,ekef,lelke,elke,elkef,tstep); ----
                              else:
                              [   #  initial and final energy are in different interpolation bins, 
                                  #  calc range from ekef to E(lelkef+1) and from E(lelke) to eke  
                                  #  and add the pre-calculated range from E(lelkef+1) to E(lelke) 
                                  ekei = E_array(lelke,medium)
                                  elkei = (lelke - eke0(medium))/eke1(medium)
                                  # --- Inline replace: $ COMPUTE_DRANGE(eke,ekei,lelke,elke,elkei,tuss); -----
                                  if compute_drange:
                                      tuss = compute_drange(eke1, eke2, lelke1, elke1, elke2)
                                  else:
        
                                      fedep = 1 - ekei/eke
                                      elktmp = 0.5*(elke+elkei+0.25*fedep*fedep*(1+fedep*(1+0.875*fedep)))
                                               #  the above evaluates the logarithm of the midpoint energy
                                      lelktmp = lelke
                                      if lelec < 0:

                                           dedxmid = ededx1[Lelktmp,MEDIUM]*elktmp+ ededx0[Lelktmp,MEDIUM]  # EVALUATE dedxmid USING ededx(elktmp)
                                          dedxmid = 1/dedxmid
                                          aux = ededx1(lelktmp,medium)*dedxmid
                                          # aux = ededx1(lelktmp,medium)/dedxmid
                                      else:
                                           dedxmid = pdedx1[Lelktmp,MEDIUM]*elktmp+ pdedx0[Lelktmp,MEDIUM]  # EVALUATE dedxmid USING pdedx(elktmp)
                                          dedxmid = 1/dedxmid
                                          aux = pdedx1(lelktmp,medium)*dedxmid
                                          # aux = pdedx1(lelktmp,medium)/dedxmid

                                      aux = aux*(1+2*aux)*(fedep/(2-fedep))**2/6
                                      # tuss = fedep*eke/dedxmid*(1+aux)
                                      tuss = fedep*eke*dedxmid*(1+aux)
                                  # End inline replace: $ COMPUTE_DRANGE(eke,ekei,lelke,elke,elkei,tuss); ----
                                  ekei = E_array(lelkef+1,medium)
                                  elkei = (lelkef + 1 - eke0(medium))/eke1(medium)
                                  # --- Inline replace: $ COMPUTE_DRANGE(ekei,ekef,lelkef,elkei,elkef,tstep); -----
                                  if compute_drange:
                                      tstep = compute_drange(eke1, eke2, lelke1, elke1, elke2)
                                  else:
        
                                      fedep = 1 - ekef/ekei
                                      elktmp = 0.5*(elkei+elkef+0.25*fedep*fedep*(1+fedep*(1+0.875*fedep)))
                                               #  the above evaluates the logarithm of the midpoint energy
                                      lelktmp = lelkef
                                      if lelec < 0:

                                           dedxmid = ededx1[Lelktmp,MEDIUM]*elktmp+ ededx0[Lelktmp,MEDIUM]  # EVALUATE dedxmid USING ededx(elktmp)
                                          dedxmid = 1/dedxmid
                                          aux = ededx1(lelktmp,medium)*dedxmid
                                          # aux = ededx1(lelktmp,medium)/dedxmid
                                      else:
                                           dedxmid = pdedx1[Lelktmp,MEDIUM]*elktmp+ pdedx0[Lelktmp,MEDIUM]  # EVALUATE dedxmid USING pdedx(elktmp)
                                          dedxmid = 1/dedxmid
                                          aux = pdedx1(lelktmp,medium)*dedxmid
                                          # aux = pdedx1(lelktmp,medium)/dedxmid

                                      aux = aux*(1+2*aux)*(fedep/(2-fedep))**2/6
                                      # tstep = fedep*ekei/dedxmid*(1+aux)
                                      tstep = fedep*ekei*dedxmid*(1+aux)
                                  # End inline replace: $ COMPUTE_DRANGE(ekei,ekef,lelkef,elkei,elkef,tstep); ----
                                  tstep=tstep+tuss+
                                          range_ep(qel,lelke,medium)-range_ep(qel,lelkef+1,medium)


                            total_tstep = tstep
                            compute_tstep = False

                          tstep = total_tstep/rhof #  non-default density scaling 
                    # End inline replace: $ CALCULATE_TSTEP_FROM_DEMFP; ----
                ] # end sig if-else

                # calculate stopping power
                if lelec < 0:
                     dedx0 = ededx1[Lelke,MEDIUM*elke+ ededx0[Lelke,MEDIUM]  # EVALUATE dedx0 USING ededx(elke)] # e-
                else:
                     dedx0 = pdedx1[Lelke,MEDIUM]*elke+ pdedx0[Lelke,MEDIUM]  # EVALUATE dedx0 USING pdedx(elke) # e+
                dedx  = rhof*dedx0

                # Determine maximum step-size (Formerly $ SET-TUSTEP) 
                 tmxs = tmxs1[Lelke,MEDIUM]*elke+ tmxs0[Lelke,MEDIUM]  # EVALUATE tmxs USING tmxs(elke)
                tmxs = tmxs/rhof

                # Compute the range to E_min(medium) (e_min is the first
                # energy in the table). Do not go more than range.
                # Don't replace this macro and don't override range, because
                # the energy loss evaluation below relies on the accurate
                # (and self-consistent) evaluation of range!
                # --- Inline replace: $ COMPUTE_RANGE; -----
                if compute_range:
                    compute_range()
                else:
                    
                    #         ===============
                      if  do_range :

                          ekei = E_array(lelke,medium)
                          elkei = (lelke - eke0(medium))/eke1(medium)
                          # --- Inline replace: $ COMPUTE_DRANGE(eke,ekei,lelke,elke,elkei,range); -----
                          if compute_drange:
                              range = compute_drange(eke1, eke2, lelke1, elke1, elke2)
                          else:
        
                              fedep = 1 - ekei/eke
                              elktmp = 0.5*(elke+elkei+0.25*fedep*fedep*(1+fedep*(1+0.875*fedep)))
                                       #  the above evaluates the logarithm of the midpoint energy
                              lelktmp = lelke
                              if lelec < 0:

                                   dedxmid = ededx1[Lelktmp,MEDIUM]*elktmp+ ededx0[Lelktmp,MEDIUM]  # EVALUATE dedxmid USING ededx(elktmp)
                                  dedxmid = 1/dedxmid
                                  aux = ededx1(lelktmp,medium)*dedxmid
                                  # aux = ededx1(lelktmp,medium)/dedxmid
                              else:
                                   dedxmid = pdedx1[Lelktmp,MEDIUM]*elktmp+ pdedx0[Lelktmp,MEDIUM]  # EVALUATE dedxmid USING pdedx(elktmp)
                                  dedxmid = 1/dedxmid
                                  aux = pdedx1(lelktmp,medium)*dedxmid
                                  # aux = pdedx1(lelktmp,medium)/dedxmid

                              aux = aux*(1+2*aux)*(fedep/(2-fedep))**2/6
                              # range = fedep*eke/dedxmid*(1+aux)
                              range = fedep*eke*dedxmid*(1+aux)
                          # End inline replace: $ COMPUTE_DRANGE(eke,ekei,lelke,elke,elkei,range); ----
                          the_range = range + range_ep(qel,lelke,medium)
                          do_range = False

                      range = the_range/rhof
                # End inline replace: $ COMPUTE_RANGE; ----

                # The RANDOMIZE-TUSTEP option as coded by AFB forced the
                # electrons to approach discrete events (Moller,brems etc.)
                # only in a single scattering mode => waste of CPU time.
                # Moved here and changed by IK Oct 22 1997
                random_tustep = RANDOMIZE_TUSTEP
                if random_tustep:

                    # --- Inline replace: $ RANDOMSET rnnotu; -----
                    if randomset:
                        rnnotu = randomset()
                    else:
                        
                          if  rng_seed > 24 :

                              call ranlux(rng_array); rng_seed = 1

                           rnnotu = rng_array(rng_seed)
                          rng_seed = rng_seed + 1
                        
                    # End inline replace: $ RANDOMSET rnnotu; ----
                    tmxs = rnnotu*min(tmxs,smaxir(irl))
                else:

                    tmxs = min(tmxs,smaxir(irl))

                tustep = min(tstep,tmxs,range)
 # optional tustep restriction in EM field

                # --- Inline replace: $ CALL_HOWNEAR(tperp); -----
                if call_hownear:
                    <XXX> = call_hownear(tperp)
                else:
    
                    OUTPUT 35 # 35 in decimal is ascii code for the pound sign
                    (
                        ' '/
                        ' '/
                        ' ***************************************************************'/
                        ' ***************************************************************'/
                        ' '/
                        ' PRESTA-II is aborting execution because you have not defined   '/
                        ' the HOWNEAR macro for your geometry.                           '/
                        ' '/
                        ' You MUST either do so or employ a limited form of PRESTA-II    '/
                        ' which does not attempt the refined boundary crossing or lateral'/
                        ' correlation features of the algorithm.                         '/
                        ' '/
                        ' If you include the following macro in your usercode:           '/
                        ' '/
                        '                        '/
                        ' '/
                        ' you can choose between single scattering mode (very slow) and  '/
                        ' standard EGS4 mode (no PRESTA enhancments) by the appropriate  '/
                        ' choice of the parameters in your input file (see the PRESTA-II '/
                        ' manual)                                                        '/
                        ' '/
                        ' ***************************************************************'/
                        ' ***************************************************************'/
                        ' '/
                        ' '/
                    )
                    stop
                # End inline replace: $ CALL_HOWNEAR(tperp); ----
                dnear[np] = tperp
                # --- Inline replace: $ RANGE_DISCARD; -----
                if range_discard:
                    range_discard()
                else:
    
                    ;IF( i_do_rr(irl) = 1 and e[np] < e_max_rr(irl) ) [
                        if tperp >= range:
                             [# particle cannot escape local region
                            idisc = 50 + 49*iq[np] # 1 for electrons, 99 for positrons
                            go to :USER-ELECTRON-DISCARD: 


                # End inline replace: $ RANGE_DISCARD; ----       # optional regional range rejection for
                                      # particles below e_max_rr if i_do_rr set

 # default is ;, but user may implement

                # --- Inline replace: $ SET_SKINDEPTH(eke,elke); -----
                if set_skindepth:
                    <XXX> = set_skindepth(eke, elke)
                else:
    
                    # --- Inline replace: $ CALCULATE_ELASTIC_SCATTERING_MFP(ssmfp,eke,elke); -----
                    if calculate_elastic_scattering_mfp:
                        <XXX> = calculate_elastic_scattering_mfp(ssmfp, eke, elke)
                    else:
        
                        blccl = rhof*blcc(medium)
                        xccl  = rhof*xcc(medium)
                        p2 = eke*(eke+rmt2); beta2 = p2/(p2 + rmsq)
                        if  spin_effects :

                          if lelec < 0:
                                etap = etae_ms1[Lelke,MEDIUM]*elke+ etae_ms0[Lelke,MEDIUM]  # EVALUATE etap USING etae_ms(elke) 
                          else:
                                etap = etap_ms1[Lelke,MEDIUM]*elke+ etap_ms0[Lelke,MEDIUM]  # EVALUATE etap USING etap_ms(elke) 
                           ms_corr = blcce1[Lelke,MEDIUM]*elke+ blcce0[Lelke,MEDIUM]  # EVALUATE ms_corr USING blcce(elke)
                          blccl = blccl/etap/(1+0.25*etap*xccl/blccl/p2)*ms_corr

                        ssmfp=beta2/blccl
                    # End inline replace: $ CALCULATE_ELASTIC_SCATTERING_MFP(ssmfp,eke,elke); ----
                    skindepth = skindepth_for_bca*ssmfp
                # End inline replace: $ SET_SKINDEPTH(eke,elke); ----
                  # This macro sets the minimum step size for a condensed
                  # history (CH) step. When the exact BCA is used, the minimum
                  # CH step is determined by efficiency considerations only
                  # At about 3 elastic MFP's single scattering becomes more
                  # efficient than CH and so the algorithm switches off CH
                  # If one of the various inexact BCA's is invoked, this macro
                  # provides a simple way to include more sophisticated
                  # decisions about the maximum acceptable approximated CH step

                tustep = min(tustep,max(tperp,skindepth))

                # The transport logic below is determined by the logical
                # variables callhhowfar, domultiple and dosingle
                # 
                # There are the following possibilities:
                # 
                #    callhowfar = False  This indicates that the
                #    ====================  intended step is shorter than tperp
                #                          independent of BCA used
                #   - domultiple = False dosingle = False and
                #                          callmsdist = True
                #        ==> everything has been done in msdist
                #   - domultiple = True and dosingle = False
                #        ==> should happen only if exact_bca  is False
                #            indicates that MS remains to be done
                #   - domultiple = False and dosingle = True
                #        ==> should happen only if exact_bca  is True
                #            sampled distance to a single scattering event is
                #            shorter than tperp ==> do single scattering at the
                #            end of the step
                #   - domultiple = True and dosingle = True
                #        ==> error condition, something with the logic is wrong!
                # 
                #    callhowfar = True This indicates that the intended step
                #    =================== is longer than tperp and forces a
                #                        call to hawfar which returns the
                #                        straight line distance to the boundary
                #                        in the initial direction of motion
                #                        (via a modification of ustep)
                #   - domultiple = False and dosingle = False
                #        ==> should happen only of exact_bca=True
                #            simply put the particle on the boundary
                #   - domultiple = False and dosingle = True
                #        ==> should happen only of exact_bca=True
                #            single elastic scattering has to be done
                #   - domultiple = True and dosingle = False
                #        ==> should happen only of exact_bca=False
                #            indicates that MS remains to be done
                #   - domultiple = True and dosingle = True
                #        ==> error condition, something with the logic is wrong!

                # IF(tustep <= tperp and tustep > skindepth)
                # This statement changed to be consistent with PRESTA-I
                count_all_steps = count_all_steps + 1
                is_ch_step = False
                if (tustep <= tperp) and ((~exact_bca) or (tustep > skindepth)):

                    # We are further way from a boundary than a skindepth, so
                    # perform a normal condensed-history step
                    callhowfar = False # Do not call HAWFAR
                    domultiple = False # Multiple scattering done here
                    dosingle   = False # MS => no single scattering
                    callmsdist = True # Remember that msdist has been called

                    # Fourth order technique for de
                    # --- Inline replace: $ COMPUTE_ELOSS_G(tustep,eke,elke,lelke,de); -----
                    if compute_eloss_g:
                        <XXX> = compute_eloss_g(tustep, eke, elke, lelke, de)
                    else:
    
                        tuss = range - range_ep(qel,lelke,medium)/rhof
                          #  here tuss is the range between the initial energy and the next lower 
                          #  energy on the interpolation grid 
                        if  tuss >= tustep :
                             [  #  Final energy is in the same interpolation bin 
                            # --- Inline replace: $ COMPUTE_ELOSS(tustep,eke,elke,lelke,de); -----
                            if compute_eloss:
                                <XXX> = compute_eloss(tustep, eke, elke, lelke, de)
                            else:
                                
                                  if  lelec < 0 :

                                       dedxmid = ededx1[Lelke,MEDIUM]*elke+ ededx0[Lelke,MEDIUM]  # EVALUATE dedxmid USING ededx(elke)
                                      aux = ededx1(lelke,medium)/dedxmid
                                  else:
                                       dedxmid = pdedx1[Lelke,MEDIUM]*elke+ pdedx0[Lelke,MEDIUM]  # EVALUATE dedxmid USING pdedx(elke)
                                      aux = pdedx1(lelke,medium)/dedxmid

                                  /*
                                  de = dedxmid*tustep #  Energy loss using stopping power at the beginning 
                                  */
                                  de = dedxmid*tustep*rhof # IK: rhof scaling bug, June 9 2006
                                                            # rhof scaling must be done here and NOT in 
                                                            # $ COMPUTE-ELOSS-G below! 
                                  fedep = de/eke
                                  de = de*(1-0.5*fedep*aux*(1-0.333333*fedep*(aux-1-
                                             0.25*fedep*(2-aux*(4-aux)))))
                            # End inline replace: $ COMPUTE_ELOSS(tustep,eke,elke,lelke,de); ----
                            /* de = de*rhof # IK, rhof bug  */
                            # IK: rhof scaling bug, June 9 2006. rhof scaling is done in 
                            #     $ COMPUTE-ELOSS above!                                   
                        else:
                               #  pre-calculated ranges                                     
                            lelktmp = lelke
                            tuss = (range - tustep)*rhof
                               #  now tuss is the range of the final energy electron 
                               #  scaled to the default mass density from PEGS4      
                            if  tuss <= 0 :
                                 de = eke - TE(medium)*0.99 
                              #  i.e., if the step we intend to take is longer than the particle 
                              #  range, the particle energy goes down to the threshold 
                              # (eke is the initial particle energy)  
                              # originally the entire energy was lost, but msdist_xxx is not prepared
                              # to deal with such large eloss fractions => changed July 2005.
                            else:
                                WHILE ( tuss < range_ep(qel,lelktmp,medium) ) [
                                    lelktmp = lelktmp - 1; ]
                                elktmp = (lelktmp+1-eke0(medium))/eke1(medium)
                                eketmp = E_array(lelktmp+1,medium)
                                # tuss = range_ep(qel,lelktmp+1,medium) - tuss
                                # IK: rhof scaling bug, June 9 2006: because of the change in 
                                #     $ COMPUTE-ELOSS above, we must scale tuss by rhof         
                                tuss = (range_ep(qel,lelktmp+1,medium) - tuss)/rhof
                                # --- Inline replace: $ COMPUTE_ELOSS(tuss,eketmp,elktmp,lelktmp,de); -----
                                if compute_eloss:
                                    <XXX> = compute_eloss(tuss, eketmp, elktmp, lelktmp, de)
                                else:
                                    
                                      if  lelec < 0 :

                                           dedxmid = ededx1[Lelktmp,MEDIUM]*elktmp+ ededx0[Lelktmp,MEDIUM]  # EVALUATE dedxmid USING ededx(elktmp)
                                          aux = ededx1(lelktmp,medium)/dedxmid
                                      else:
                                           dedxmid = pdedx1[Lelktmp,MEDIUM]*elktmp+ pdedx0[Lelktmp,MEDIUM]  # EVALUATE dedxmid USING pdedx(elktmp)
                                          aux = pdedx1(lelktmp,medium)/dedxmid

                                      /*
                                      de = dedxmid*tuss #  Energy loss using stopping power at the beginning 
                                      */
                                      de = dedxmid*tuss*rhof # IK: rhof scaling bug, June 9 2006
                                                                # rhof scaling must be done here and NOT in 
                                                                # $ COMPUTE-ELOSS-G below! 
                                      fedep = de/eketmp
                                      de = de*(1-0.5*fedep*aux*(1-0.333333*fedep*(aux-1-
                                                 0.25*fedep*(2-aux*(4-aux)))))
                                # End inline replace: $ COMPUTE_ELOSS(tuss,eketmp,elktmp,lelktmp,de); ----
                                de = de + eke - eketmp


                    # End inline replace: $ COMPUTE_ELOSS_G(tustep,eke,elke,lelke,de); ----

                    tvstep = tustep; is_ch_step = True

                    if transport_algorithm == PRESTA_II:

                      call msdist_pII
                      (
                        # Inputs
                        eke,de,tustep,rhof,medium,qel,spin_effects,
                        u[np],v[np],w[np],x[np],y[np],z[np],
                        # Outputs
                        uscat,vscat,wscat,xtrans,ytrans,ztrans,ustep
                      )
                    else:

                      call msdist_pI
                      (
                        # Inputs
                        eke,de,tustep,rhof,medium,qel,spin_effects,
                        u[np],v[np],w[np],x[np],y[np],z[np],
                        # Outputs
                        uscat,vscat,wscat,xtrans,ytrans,ztrans,ustep
                      )

                else:

                    # We are within a skindepth from a boundary, invoke
                    # one of the various boundary-crossing algorithms
                    callmsdist = False
                         # Remember that msdist has not been called
                    if exact_bca:

                        # Cross the boundary in a single scattering mode
                        domultiple = False # Do not do multiple scattering
                        # Sample the distance to a single scattering event
                        # --- Inline replace: $ RANDOMSET rnnoss; -----
                        if randomset:
                            rnnoss = randomset()
                        else:
                            
                              if  rng_seed > 24 :

                                  call ranlux(rng_array); rng_seed = 1

                               rnnoss = rng_array(rng_seed)
                              rng_seed = rng_seed + 1
                            
                        # End inline replace: $ RANDOMSET rnnoss; ----
                        if  rnnoss < 1.e-30 :

                            rnnoss = 1.e-30

                        lambda_ = - Log(1 - rnnoss)
                        lambda_max = 0.5*blccl*rm/dedx*(eke/rm+1)**3
                        if  lambda_ >= 0 and lambda_max > 0 :

                            if  lambda_ < lambda_max :

                                tuss=lambda_*ssmfp*(1-0.5*lambda_/lambda_max)
                            else:
                              tuss = 0.5 * lambda_ * ssmfp

                            if tuss < tustep:

                                tustep = tuss
                                dosingle = True
                            else:
                                dosingle = False

                        else:
                          $egs_warning(*,' lambda_ > lambda_max: ',
                             lambda_,lambda_max,' eke dedx: ',eke,dedx,
                             ' ir medium blcc: ',ir[np],medium,blcc(medium),
                             ' position = ',x[np],y[np],z[np])
                          dosingle = False
                          np=np-1; return

                        ustep = tustep
                    else:

                        # Boundary crossing a la EGS4/PRESTA-I but using
                        # exact PLC
                        dosingle = False
                        domultiple = True
                        # --- Inline replace: $ SET_USTEP; -----
                        if set_ustep:
                            set_ustep()
                        else:
    
                            ekems = eke - 0.5*tustep*dedx # Use mid-point energy to calculate
                                                            # energy dependent quantities
                            # --- Inline replace: $ CALCULATE_XI(tustep); -----
                            if calculate_xi:
                                <XXX> = calculate_xi(tustep)
                            else:
        
                                p2 = ekems*(ekems+rmt2); beta2 = p2/(p2 + rmsq)
                                chia2 = xccl/(4*blccl*p2)
                                                              # Note that our chia2 is Moliere chia2/4
                                                              # Note also that xcc is now old egs xcc**2
                                xi = 0.5*xccl/p2/beta2*tustep
                                if  spin_effects :

                                    elkems = Log(ekems)
                                    # Unhandled macro '$ SET INTERVAL elkems,eke;'
                                    if lelec < 0:

                                         etap = etae_ms1[Lelkems,MEDIUM]*elkems+ etae_ms0[Lelkems,MEDIUM]  # EVALUATE etap USING etae_ms(elkems)
                                         xi_corr = q1ce_ms1[Lelkems,MEDIUM]*elkems+ q1ce_ms0[Lelkems,MEDIUM]  # EVALUATE xi_corr USING q1ce_ms(elkems)
                                    else:
                                         etap = etap_ms1[Lelkems,MEDIUM]*elkems+ etap_ms0[Lelkems,MEDIUM]  # EVALUATE etap USING etap_ms(elkems)
                                         xi_corr = q1cp_ms1[Lelkems,MEDIUM]*elkems+ q1cp_ms0[Lelkems,MEDIUM]  # EVALUATE xi_corr USING q1cp_ms(elkems)

                                    chia2 = chia2*etap; xi = xi*xi_corr
                                     ms_corr = blcce1[Lelkems,MEDIUM]*elkems+ blcce0[Lelkems,MEDIUM]  # EVALUATE ms_corr USING blcce(elkems)
                                    blccl = blccl*ms_corr
                                else:
                                     xi_corr = 1; etap = 1 
                                xi = xi*(Log(1+1./chia2)-1/(1+chia2))
                            # End inline replace: $ CALCULATE_XI(tustep); ----
                            if  xi < 0.1 :

                                ustep = tustep*(1 - xi*(0.5 - xi*0.166667))
                              else:

                                ustep = tustep*(1 - Exp(-xi))/xi

                        # End inline replace: $ SET_USTEP; ----

                    if ustep < tperp:

                        callhowfar = False
                    else:

                        callhowfar = True


            ] # end non-vacuum test

 # additional ustep restriction in em field
                                  # default for $ SET-USTEP-EM-FIELD; is ;(null) 
            irold  = ir[np] # save current region
            irnew  = ir[np] # default new region is current region
            idisc  = 0 # default is no discard (this flag is initialized here)
            ustep0 = ustep # Save the intended ustep.

            # IF(callhowfar) [ call howfar; ]
            # --- Inline replace: $ CALL_HOWFAR_IN_ELECTR; -----
            if call_howfar_in_electr:
                call_howfar_in_electr()
            else:
                
                  if callhowfar or wt[np] <= 0:
                       call howfar 
            # End inline replace: $ CALL_HOWFAR_IN_ELECTR; ---- # The above is the default replacement

            # Now see if user requested discard
            if idisc > 0) # (idisc is returned by howfar:

                # User requested immediate discard
                go to :USER-ELECTRON-DISCARD:

            # --- Inline replace: $ CHECK_NEGATIVE_USTEP; -----
            if check_negative_ustep:
                check_negative_ustep()
            else:
                
                    if ustep <= 0:

                        # Negative ustep---probable truncation problem at a
                        # boundary, which means we are not in the region we think
                        # we are in.  The default macro assumes that user has set
                        # irnew to the region we are really most likely to be
                        # in.  A message is written out whenever ustep is less than -1.e-4
                        if ustep < -1e-4:

                            ierust = ierust + 1
                            OUTPUT ierust,ustep,dedx,e[np]-prm,
                                   ir[np],irnew,irold,x[np],y[np],z[np]
                            (i4,' Negative ustep = ',e12.5,' dedx=',F8.4,' ke=',F8.4,
                             ' ir,irnew,irold =',3i4,' x,y,z =',4e10.3)
                            if ierust > 1000:

                                OUTPUT;(////' Called exit---too many ustep errors'///)
                                $CALL_EXIT(1)


                        ustep = 0

            # End inline replace: $ CHECK_NEGATIVE_USTEP; ----

            if ustep == 0 or medium = 0:

                # Do fast step in vacuum
                if ustep != 0:

                    IF EM_MACROS_ACTIVE

                        edep = pzero # no energy loss in vacuum
                        # transport in EMF in vacuum:
                        # only a B or and E field can be active
                        # (not both at the same time)

                    else:

                        # Step in vacuum
                        vstep  = ustep
                        tvstep = vstep
                        # ( vstep is ustep truncated (possibly) by howfar
                        #  tvstep is the total curved path associated with vstep)
                        edep = pzero # no energy loss in vacuum

                            # additional vacuum transport in em field
                        e_range = vacdst
IARG=TRANAUSB ;  IF (IAUSFL(IARG+1).NE.0) [CALL AUSGAB(IARG);]
                        # Transport the particle
                        x[np] = x[np] + u[np]*vstep
                        y[np] = y[np] + v[np]*vstep
                        z[np] = z[np] + w[np]*vstep
                        dnear[np] = dnear[np] - vstep
                            # (dnear is distance to the nearest boundary
                            #  that goes along with particle stack and
                            #  which the user's howfar can supply (option)

                            # default for $ SET-ANGLES-EM-FIELD; is ; (null) 
                             # (allows for EM field deflection
                    ] # end of EM_MACROS_ACTIVE block
                ] # end of vacuum step

                if irnew != irold:

                     [ # --- Inline replace: $ electron_region_change; -----
 if electron_region_change:
     electron_region_change()
 else:
    
     ir[np] = irnew; irl = irnew; medium = med(irl)
 # End inline replace: $ electron_region_change; ---- ]

                if ustep != 0) [IARG=TRANAUSA ;  IF (IAUSFL(IARG+1) != 0:

                    CALL AUSGAB(IARG)]
                if eie <= ecut(irl):
                    go to :ECUT-DISCARD:
                if ustep != 0 and idisc < 0:
                    go to :USER-ELECTRON-DISCARD:
                NEXT :TSTEP:  # (Start again at :TSTEP:)

            ] # Go try another big step in (possibly) new medium

            vstep = ustep

            if callhowfar:

                if exact_bca:

                    # if callhowfar is True and exact_bca=True we are
                    # in a single scattering mode
                    tvstep = vstep
                    if tvstep != tustep:

                       # Boundary was crossed. Shut off single scattering
                        dosingle = False

                else:

                    # callhowfar=True and exact_bca=False
                    # =>we are doing an approximate CH step
                    # calculate the average curved path-length corresponding
                    # to vstep
                    # --- Inline replace: $ SET_TVSTEP; -----
                    if set_tvstep:
                        set_tvstep()
                    else:
    
                        ;IF ( vstep < ustep0 )

                          ekems = eke - 0.5*tustep*vstep/ustep0*dedx
                             # This estimates the energy loss to the boundary.
                             # tustep was the intended curved path-length,
                             # ustep0 is the average transport distance in the initial direction
                             #        resulting from tustep
                             # vstep = ustep is the reduced average transport distance in the 
                             #               initial direction due to boundary crossing
                          # --- Inline replace: $ CALCULATE_XI(vstep); -----
                          if calculate_xi:
                              <XXX> = calculate_xi(vstep)
                          else:
        
                              p2 = ekems*(ekems+rmt2); beta2 = p2/(p2 + rmsq)
                              chia2 = xccl/(4*blccl*p2)
                                                            # Note that our chia2 is Moliere chia2/4
                                                            # Note also that xcc is now old egs xcc**2
                              xi = 0.5*xccl/p2/beta2*vstep
                              if  spin_effects :

                                  elkems = Log(ekems)
                                  # Unhandled macro '$ SET INTERVAL elkems,eke;'
                                  if lelec < 0:

                                       etap = etae_ms1[Lelkems,MEDIUM]*elkems+ etae_ms0[Lelkems,MEDIUM]  # EVALUATE etap USING etae_ms(elkems)
                                       xi_corr = q1ce_ms1[Lelkems,MEDIUM]*elkems+ q1ce_ms0[Lelkems,MEDIUM]  # EVALUATE xi_corr USING q1ce_ms(elkems)
                                  else:
                                       etap = etap_ms1[Lelkems,MEDIUM]*elkems+ etap_ms0[Lelkems,MEDIUM]  # EVALUATE etap USING etap_ms(elkems)
                                       xi_corr = q1cp_ms1[Lelkems,MEDIUM]*elkems+ q1cp_ms0[Lelkems,MEDIUM]  # EVALUATE xi_corr USING q1cp_ms(elkems)

                                  chia2 = chia2*etap; xi = xi*xi_corr
                                   ms_corr = blcce1[Lelkems,MEDIUM]*elkems+ blcce0[Lelkems,MEDIUM]  # EVALUATE ms_corr USING blcce(elkems)
                                  blccl = blccl*ms_corr
                              else:
                                   xi_corr = 1; etap = 1 
                              xi = xi*(Log(1+1./chia2)-1/(1+chia2))
                          # End inline replace: $ CALCULATE_XI(vstep); ----
                          if  xi < 0.1 :

                            tvstep = vstep*(1 + xi*(0.5 + xi*0.333333))
                          else:

                            if  xi < 0.999999 :

                               tvstep = -vstep*Log(1 - xi)/xi
                            else:

                               # This is an error condition because the average transition 
                               # in the initial direction of motion is always smaller than 1/Q1
                               logger.info({P2})
                               logger.info({P2})
                               logger.info({P2})
                               logger.info({P2})
                               logger.info({P2})
                               logger.info({P2})
                               logger.info({P2})
                               logger.info({P2})
                               logging.critical('***************** Error: ')
    
                                       logging.critical('{P2}')
    
                                       logging.critical('***************** Quitting now.')
    
                                       sys.exit(1)


                        else:

                          tvstep = tustep

                    # End inline replace: $ SET_TVSTEP; ----

                # Fourth order technique for dedx
                # Must be done for an approx. CH step or a
                # single scattering step.
                # --- Inline replace: $ COMPUTE_ELOSS_G(tvstep,eke,elke,lelke,de); -----
                if compute_eloss_g:
                    <XXX> = compute_eloss_g(tvstep, eke, elke, lelke, de)
                else:
    
                    tuss = range - range_ep(qel,lelke,medium)/rhof
                      #  here tuss is the range between the initial energy and the next lower 
                      #  energy on the interpolation grid 
                    if  tuss >= tvstep :
                         [  #  Final energy is in the same interpolation bin 
                        # --- Inline replace: $ COMPUTE_ELOSS(tvstep,eke,elke,lelke,de); -----
                        if compute_eloss:
                            <XXX> = compute_eloss(tvstep, eke, elke, lelke, de)
                        else:
                            
                              if  lelec < 0 :

                                   dedxmid = ededx1[Lelke,MEDIUM]*elke+ ededx0[Lelke,MEDIUM]  # EVALUATE dedxmid USING ededx(elke)
                                  aux = ededx1(lelke,medium)/dedxmid
                              else:
                                   dedxmid = pdedx1[Lelke,MEDIUM]*elke+ pdedx0[Lelke,MEDIUM]  # EVALUATE dedxmid USING pdedx(elke)
                                  aux = pdedx1(lelke,medium)/dedxmid

                              /*
                              de = dedxmid*tvstep #  Energy loss using stopping power at the beginning 
                              */
                              de = dedxmid*tvstep*rhof # IK: rhof scaling bug, June 9 2006
                                                        # rhof scaling must be done here and NOT in 
                                                        # $ COMPUTE-ELOSS-G below! 
                              fedep = de/eke
                              de = de*(1-0.5*fedep*aux*(1-0.333333*fedep*(aux-1-
                                         0.25*fedep*(2-aux*(4-aux)))))
                        # End inline replace: $ COMPUTE_ELOSS(tvstep,eke,elke,lelke,de); ----
                        /* de = de*rhof # IK, rhof bug  */
                        # IK: rhof scaling bug, June 9 2006. rhof scaling is done in 
                        #     $ COMPUTE-ELOSS above!                                   
                    else:
                           #  pre-calculated ranges                                     
                        lelktmp = lelke
                        tuss = (range - tvstep)*rhof
                           #  now tuss is the range of the final energy electron 
                           #  scaled to the default mass density from PEGS4      
                        if  tuss <= 0 :
                             de = eke - TE(medium)*0.99 
                          #  i.e., if the step we intend to take is longer than the particle 
                          #  range, the particle energy goes down to the threshold 
                          # (eke is the initial particle energy)  
                          # originally the entire energy was lost, but msdist_xxx is not prepared
                          # to deal with such large eloss fractions => changed July 2005.
                        else:
                            WHILE ( tuss < range_ep(qel,lelktmp,medium) ) [
                                lelktmp = lelktmp - 1; ]
                            elktmp = (lelktmp+1-eke0(medium))/eke1(medium)
                            eketmp = E_array(lelktmp+1,medium)
                            # tuss = range_ep(qel,lelktmp+1,medium) - tuss
                            # IK: rhof scaling bug, June 9 2006: because of the change in 
                            #     $ COMPUTE-ELOSS above, we must scale tuss by rhof         
                            tuss = (range_ep(qel,lelktmp+1,medium) - tuss)/rhof
                            # --- Inline replace: $ COMPUTE_ELOSS(tuss,eketmp,elktmp,lelktmp,de); -----
                            if compute_eloss:
                                <XXX> = compute_eloss(tuss, eketmp, elktmp, lelktmp, de)
                            else:
                                
                                  if  lelec < 0 :

                                       dedxmid = ededx1[Lelktmp,MEDIUM]*elktmp+ ededx0[Lelktmp,MEDIUM]  # EVALUATE dedxmid USING ededx(elktmp)
                                      aux = ededx1(lelktmp,medium)/dedxmid
                                  else:
                                       dedxmid = pdedx1[Lelktmp,MEDIUM]*elktmp+ pdedx0[Lelktmp,MEDIUM]  # EVALUATE dedxmid USING pdedx(elktmp)
                                      aux = pdedx1(lelktmp,medium)/dedxmid

                                  /*
                                  de = dedxmid*tuss #  Energy loss using stopping power at the beginning 
                                  */
                                  de = dedxmid*tuss*rhof # IK: rhof scaling bug, June 9 2006
                                                            # rhof scaling must be done here and NOT in 
                                                            # $ COMPUTE-ELOSS-G below! 
                                  fedep = de/eketmp
                                  de = de*(1-0.5*fedep*aux*(1-0.333333*fedep*(aux-1-
                                             0.25*fedep*(2-aux*(4-aux)))))
                            # End inline replace: $ COMPUTE_ELOSS(tuss,eketmp,elktmp,lelktmp,de); ----
                            de = de + eke - eketmp


                # End inline replace: $ COMPUTE_ELOSS_G(tvstep,eke,elke,lelke,de); ----
            else:

               # callhowfar=False => step has not been reduced due to
               #                       boundaries
               tvstep = tustep
               if  ~callmsdist :

                  # Second order technique for dedx
                  # Already done in a normal CH step with call to msdist
                  # --- Inline replace: $ COMPUTE_ELOSS_G(tvstep,eke,elke,lelke,de); -----
                  if compute_eloss_g:
                      <XXX> = compute_eloss_g(tvstep, eke, elke, lelke, de)
                  else:
    
                      tuss = range - range_ep(qel,lelke,medium)/rhof
                        #  here tuss is the range between the initial energy and the next lower 
                        #  energy on the interpolation grid 
                      if  tuss >= tvstep :
                           [  #  Final energy is in the same interpolation bin 
                          # --- Inline replace: $ COMPUTE_ELOSS(tvstep,eke,elke,lelke,de); -----
                          if compute_eloss:
                              <XXX> = compute_eloss(tvstep, eke, elke, lelke, de)
                          else:
                              
                                if  lelec < 0 :

                                     dedxmid = ededx1[Lelke,MEDIUM]*elke+ ededx0[Lelke,MEDIUM]  # EVALUATE dedxmid USING ededx(elke)
                                    aux = ededx1(lelke,medium)/dedxmid
                                else:
                                     dedxmid = pdedx1[Lelke,MEDIUM]*elke+ pdedx0[Lelke,MEDIUM]  # EVALUATE dedxmid USING pdedx(elke)
                                    aux = pdedx1(lelke,medium)/dedxmid

                                /*
                                de = dedxmid*tvstep #  Energy loss using stopping power at the beginning 
                                */
                                de = dedxmid*tvstep*rhof # IK: rhof scaling bug, June 9 2006
                                                          # rhof scaling must be done here and NOT in 
                                                          # $ COMPUTE-ELOSS-G below! 
                                fedep = de/eke
                                de = de*(1-0.5*fedep*aux*(1-0.333333*fedep*(aux-1-
                                           0.25*fedep*(2-aux*(4-aux)))))
                          # End inline replace: $ COMPUTE_ELOSS(tvstep,eke,elke,lelke,de); ----
                          /* de = de*rhof # IK, rhof bug  */
                          # IK: rhof scaling bug, June 9 2006. rhof scaling is done in 
                          #     $ COMPUTE-ELOSS above!                                   
                      else:
                             #  pre-calculated ranges                                     
                          lelktmp = lelke
                          tuss = (range - tvstep)*rhof
                             #  now tuss is the range of the final energy electron 
                             #  scaled to the default mass density from PEGS4      
                          if  tuss <= 0 :
                               de = eke - TE(medium)*0.99 
                            #  i.e., if the step we intend to take is longer than the particle 
                            #  range, the particle energy goes down to the threshold 
                            # (eke is the initial particle energy)  
                            # originally the entire energy was lost, but msdist_xxx is not prepared
                            # to deal with such large eloss fractions => changed July 2005.
                          else:
                              WHILE ( tuss < range_ep(qel,lelktmp,medium) ) [
                                  lelktmp = lelktmp - 1; ]
                              elktmp = (lelktmp+1-eke0(medium))/eke1(medium)
                              eketmp = E_array(lelktmp+1,medium)
                              # tuss = range_ep(qel,lelktmp+1,medium) - tuss
                              # IK: rhof scaling bug, June 9 2006: because of the change in 
                              #     $ COMPUTE-ELOSS above, we must scale tuss by rhof         
                              tuss = (range_ep(qel,lelktmp+1,medium) - tuss)/rhof
                              # --- Inline replace: $ COMPUTE_ELOSS(tuss,eketmp,elktmp,lelktmp,de); -----
                              if compute_eloss:
                                  <XXX> = compute_eloss(tuss, eketmp, elktmp, lelktmp, de)
                              else:
                                  
                                    if  lelec < 0 :

                                         dedxmid = ededx1[Lelktmp,MEDIUM]*elktmp+ ededx0[Lelktmp,MEDIUM]  # EVALUATE dedxmid USING ededx(elktmp)
                                        aux = ededx1(lelktmp,medium)/dedxmid
                                    else:
                                         dedxmid = pdedx1[Lelktmp,MEDIUM]*elktmp+ pdedx0[Lelktmp,MEDIUM]  # EVALUATE dedxmid USING pdedx(elktmp)
                                        aux = pdedx1(lelktmp,medium)/dedxmid

                                    /*
                                    de = dedxmid*tuss #  Energy loss using stopping power at the beginning 
                                    */
                                    de = dedxmid*tuss*rhof # IK: rhof scaling bug, June 9 2006
                                                              # rhof scaling must be done here and NOT in 
                                                              # $ COMPUTE-ELOSS-G below! 
                                    fedep = de/eketmp
                                    de = de*(1-0.5*fedep*aux*(1-0.333333*fedep*(aux-1-
                                               0.25*fedep*(2-aux*(4-aux)))))
                              # End inline replace: $ COMPUTE_ELOSS(tuss,eketmp,elktmp,lelktmp,de); ----
                              de = de + eke - eketmp


                  # End inline replace: $ COMPUTE_ELOSS_G(tvstep,eke,elke,lelke,de); ----


 # additional path length correction in em field
                # ( Calculates tvstep given vstep
                #  default for $ SET-TVSTEP-EM-FIELD; is ; (null) 

            save_de = de # the energy loss is used to calculate the number
                              # of MFP gone up to now. If energy loss
                              # fluctuations are implemented, de will be
                              # changed in $ DE-FLUCTUATION; => save 

            # The following macro template allows the user to change the
            # ionization loss.
            # (Provides a user hook for Landau/Vavilov processes)

                # default for $ DE-FLUCTUATION; is ; (null) 
            edep = de # energy deposition variable for user
 # e-loss or gain in em field
 # EEMF implementation
                # Default for $ ADD-WORK-EM-FIELD; is ; (null) 
            ekef = eke - de # (final kinetic energy)
            eold = eie # save old value
            enew = eold - de # energy at end of transport

            # Now do multiple scattering
            if  ~callmsdist :
                   # everything done if callmsdist  is True

                if  domultiple :

                    # Approximated CH step => do multiple scattering
                    # 
                    # ekems, elkems, beta2 have been set in either $ SET-TUSTEP 
                    # or $ SET-TVSTEP if spin_effects is True, they are 
                    # not needed if spin_effects is False
                    # 
                    # chia2,etap,xi,xi_corr are also set in the above macros
                    # 
                    # qel (0 for e-, 1 for e+) and medium are now also required
                    # (for the spin rejection loop)
                    # 
                    lambda_ = blccl*tvstep/beta2/etap/(1+chia2)
                    xi = xi/xi_corr
                    findindex = True; spin_index = True
                    call mscat(lambda_,chia2,xi,elkems,beta2,qel,medium,
                               spin_effects,findindex,spin_index,
                               costhe,sinthe)
                else:

                    if dosingle:

                       # Single scattering

                       ekems = Max(ekef,ecut(irl)-rm)
                       p2 = ekems*(ekems + rmt2)
                       beta2 = p2/(p2 + rmsq)
                       chia2 = xcc(medium)/(4*blcc(medium)*p2)
                       if  spin_effects :

                         elkems = Log(ekems)
                         # Unhandled macro '$ SET INTERVAL elkems,eke;'
                         if lelec < 0:
                              etap = etae_ms1[Lelkems,MEDIUM*elkems+ etae_ms0[Lelkems,MEDIUM]  # EVALUATE etap USING etae_ms(elkems)]
                         else:
                              etap = etap_ms1[Lelkems,MEDIUM]*elkems+ etap_ms0[Lelkems,MEDIUM]  # EVALUATE etap USING etap_ms(elkems)
                         chia2 = chia2*etap

                       call sscat(chia2,elkems,beta2,qel,medium,
                                  spin_effects,costhe,sinthe)
                    else:

                       theta  = 0 # No deflection in single scattering model
                       sinthe = 0
                       costhe = 1



            # We now know distance and amount of energy loss for this step,
            # and the angle by which the electron will be scattered. Hence,
            # it is time to call the user and inform him of this transport,
            # after which we will do it.

            # Now transport, deduct energy loss, and do multiple scatter.
            e_range = range
            /******* trying to save evaluation of range.
            the_range = the_range - tvstep*rhof
            ********/

            /*
               Put expected final position and direction in common
               block variables so that they are available to the
               user for things such as scoring on a grid that is
               different from the geometry grid
            */
            if  callmsdist :

               # Deflection and scattering have been calculated/sampled in msdist
                u_final = uscat
                v_final = vscat
                w_final = wscat
                x_final = xtrans
                y_final = ytrans
                z_final = ztrans
            else:

                IF ~(EM_MACROS_ACTIVE)

                    x_final = x[np] + u[np]*vstep
                    y_final = y[np] + v[np]*vstep
                    z_final = z[np] + w[np]*vstep

                if  domultiple or dosingle :

                    u_tmp = u[np]; v_tmp = v[np]; w_tmp = w[np]
                    call uphi(2,1) # Apply the deflection, save call to uphi if
                                    # no deflection in a single scattering mode
                    u_final = u[np]; v_final = v[np]; w_final = w[np]
                    u[np] = u_tmp; v[np] = v_tmp; w[np] = w_tmp
                else:
                     u_final = u[np]; v_final = v[np]; w_final = w[np]; 

IARG=TRANAUSB ;  IF (IAUSFL(IARG+1).NE.0) [CALL AUSGAB(IARG);]

            # Transport the particle

            x[np] = x_final; y[np] = y_final; z[np] = z_final
            u[np] = u_final; v[np] = v_final; w[np] = w_final

            dnear[np] = dnear[np] - vstep
            irold = ir[np] # save previous region

            # Default for $ SET-ANGLES-EM-FIELD; is ; (null) 


            # Now done with multiple scattering,
            # update energy and see if below cut
            # below subtracts only energy deposited
            peie  = peie - edep
            # below subtracts energy deposited + work due to E field
            # peie = peie - de
            eie   = peie
            e[np] = peie

            # IF( irnew ~= irl and eie <= ecut(irl)) [
            # IK: the above is clearly a bug. If the particle energy falls 
            #     below ecut, but the particle is actually entering a new 
            #     region, the discard will happen in the current region 
            #     instead the next. If the particle is a positron, all 
            #     resulting annihilation photons will have the new position 
            #     but the old region => confusion in the geometry routine 
            #     is very likely.      Jan 27 2004 
            if  irnew == irl and eie <= ecut(irl):

               go to :ECUT-DISCARD:

            medold = medium
            if medium != 0:

                ekeold = eke; eke = eie - rm # update kinetic energy
                elke   = log(eke)
                # Unhandled macro '$ SET INTERVAL elke,eke;' # Get updated interval

            if irnew != irold:

                 [ # --- Inline replace: $ electron_region_change; -----
 if electron_region_change:
     electron_region_change()
 else:
    
     ir[np] = irnew; irl = irnew; medium = med(irl)
 # End inline replace: $ electron_region_change; ---- ]

            # After transport call to user scoring routine
IARG=TRANAUSA ;  IF (IAUSFL(IARG+1).NE.0) [CALL AUSGAB(IARG);]

            if eie <= ecut(irl):

               go to :ECUT-DISCARD:

            # Now check for deferred discard request.  May have been set
            # by either howfar, or one of the transport ausgab calls
            if idisc < 0:

              go to :USER-ELECTRON-DISCARD:

            if medium != medold:

                 NEXT :TSTEP:


                # NRCC update 87/12/08--default is null

            # --- Inline replace: $ UPDATE_DEMFP; -----
            if update_demfp:
                update_demfp()
            else:
    
                demfp = demfp - save_de*sig
                total_de = total_de - save_de
                total_tstep = total_tstep - tvstep*rhof
                if  total_tstep < 1e-9 :
                     demfp = 0 
            # End inline replace: $ UPDATE_DEMFP; ----

        if demfp < EPSEMFP:

            break  # end ustep loop

        # Compute final sigma to see if resample is needed.
        # this will take the energy variation of the sigma into
        # account using the fictitious sigma method.

        # --- Inline replace: $ EVALUATE_SIGF; -----
        if evaluate_sigf:
            evaluate_sigf()
        else:
            
              if lelec < 0:

                   sigf = esig1[Lelke,MEDIUM]*elke+ esig0[Lelke,MEDIUM]  # EVALUATE sigf USING esig(elke)
                   dedx0 = ededx1[Lelke,MEDIUM]*elke+ ededx0[Lelke,MEDIUM]  # EVALUATE dedx0 USING ededx(elke)
                  sigf = sigf/dedx0
              else:

                   sigf = psig1[Lelke,MEDIUM]*elke+ psig0[Lelke,MEDIUM]  # EVALUATE sigf USING psig(elke)
                   dedx0 = pdedx1[Lelke,MEDIUM]*elke+ pdedx0[Lelke,MEDIUM]  # EVALUATE dedx0 USING pdedx(elke)
                  sigf = sigf/dedx0

        # End inline replace: $ EVALUATE_SIGF; ----

        sigratio = sigf/sig0

        # --- Inline replace: $ RANDOMSET rfict; -----
        if randomset:
            rfict = randomset()
        else:
            
              if  rng_seed > 24 :

                  call ranlux(rng_array); rng_seed = 1

               rfict = rng_array(rng_seed)
              rng_seed = rng_seed + 1
            
        # End inline replace: $ RANDOMSET rfict; ----

    if rfict <= sigratio:

        break   # end tstep loop

    #  Now sample electron interaction

    if lelec < 0:

        # e-,check branching ratio
        # --- Inline replace: $ EVALUATE_EBREM_FRACTION; -----
        if evaluate_ebrem_fraction:
            evaluate_ebrem_fraction()
        else:
    
             ebr1 = ebr11[Lelke,MEDIUM]*elke+ ebr10[Lelke,MEDIUM]  # EVALUATE ebr1 USING ebr1(elke)
        # End inline replace: $ EVALUATE_EBREM_FRACTION; ----
          # Default is $ EVALUATE ebr1 USING ebr1(elke) 
        # --- Inline replace: $ RANDOMSET rnno24; -----
        if randomset:
            rnno24 = randomset()
        else:
            
              if  rng_seed > 24 :

                  call ranlux(rng_array); rng_seed = 1

               rnno24 = rng_array(rng_seed)
              rng_seed = rng_seed + 1
            
        # End inline replace: $ RANDOMSET rnno24; ----
        if rnno24 <= ebr1:

            # It was bremsstrahlung
            go to :EBREMS:
        else:

            # It was Moller, but first check the kinematics.
            # However, if EII is on, we should still permit an interaction
            # even if E<moller threshold as EII interactions go down to
            # the ionization threshold which may be less than thmoll.
            if e[np] <= thmoll(medium) and eii_flag == 0:
                
                 # (thmoll = lower Moller threshold)

                # Not enough energy for Moller, so
                # force it to be a bremsstrahlung---provided ok kinematically.
                if ebr1 <= 0:
                    go to :NEWELECTRON:
                    # Brems not allowed either.
                go to :EBREMS:

IARG=MOLLAUSB ;  IF (IAUSFL(IARG+1).NE.0) [CALL AUSGAB(IARG);]
            call moller
            # The following macro template allows the user to change the
            # particle selection scheme (e.g., adding importance sampling
            # such as splitting, leading particle selection, etc.).
            # (Default macro is template '$ PARTICLE-SELECTION-ELECTR' 
            # which in turn has the 'null' replacement ';')

IARG=MOLLAUSA ;  IF (IAUSFL(IARG+1).NE.0) [CALL AUSGAB(IARG);]
            if  iq[np] == 0 :
                 return

        go to :NEWELECTRON: # Electron is lowest energy-follow it

    # e+ interaction. pbr1 = brems/(brems + bhabha + annih
    # --- Inline replace: $ EVALUATE_PBREM_FRACTION; -----
    if evaluate_pbrem_fraction:
        evaluate_pbrem_fraction()
    else:
    
         pbr1 = pbr11[Lelke,MEDIUM]*elke+ pbr10[Lelke,MEDIUM]  # EVALUATE pbr1 USING pbr1(elke)
    # End inline replace: $ EVALUATE_PBREM_FRACTION; ----
       # Default is $ EVALUATE pbr1 USING pbr1(elke) 
    # --- Inline replace: $ RANDOMSET rnno25; -----
    if randomset:
        rnno25 = randomset()
    else:
        
          if  rng_seed > 24 :

              call ranlux(rng_array); rng_seed = 1

           rnno25 = rng_array(rng_seed)
          rng_seed = rng_seed + 1
        
    # End inline replace: $ RANDOMSET rnno25; ----
    if rnno25 < pbr1:
        go to :EBREMS: # It was bremsstrahlung
    # Decide between bhabha and annihilation
    # pbr2 is (brems + bhabha)/(brems + bhabha + annih)
    # --- Inline replace: $ EVALUATE_BHABHA_FRACTION; -----
    if evaluate_bhabha_fraction:
        evaluate_bhabha_fraction()
    else:
    
         pbr2 = pbr21[Lelke,MEDIUM]*elke+ pbr20[Lelke,MEDIUM]  # EVALUATE pbr2 USING pbr2(elke)
    # End inline replace: $ EVALUATE_BHABHA_FRACTION; ----
       # Default is $ EVALUATE pbr2 USING pbr2(elke) 
    if rnno25 < pbr2:

        # It is bhabha
IARG=BHABAUSB ;  IF (IAUSFL(IARG+1).NE.0) [CALL AUSGAB(IARG);]
        call bhabha
        # The following macro template allows the user to change the
        # particle selection scheme (e.g., adding importance sampling
        # such as splitting, leading particle selection, etc.).  (default
        # macro is template '$ PARTICLE-SELECTION-ELECTR' which in turn 
        # has the 'null' replacement ';')


IARG=BHABAUSA ;  IF (IAUSFL(IARG+1).NE.0) [CALL AUSGAB(IARG);]
        if  iq[np] == 0 :
             return
    else:

        # It is in-flight annihilation
IARG=ANNIHFAUSB ;  IF (IAUSFL(IARG+1).NE.0) [CALL AUSGAB(IARG);]
        call annih
        # The following macro template allows the user to change the
        # particle selection scheme (e.g., adding importance sampling
        # such as splitting, leading particle selection, etc.).  (default
        # macro is template '$ PARTICLE-SELECTION-ELECTR' which in turn 
        # has the 'null' replacement ';')


IARG=ANNIHFAUSA ;  IF (IAUSFL(IARG+1).NE.0) [CALL AUSGAB(IARG);]
        EXIT :NEWELECTRON: # i.e., in order to return to shower
        # After annihilation the gammas are bound to be the lowest energy
        # particles, so return and follow them.
    ] # end pbr2 else

] REPEAT # newelectron

return # i.e., return to shower
# ---------------------------------------------
# Bremsstrahlung-call section
# ---------------------------------------------
:EBREMS:
IARG=BREMAUSB ;  IF (IAUSFL(IARG+1).NE.0) [CALL AUSGAB(IARG);]
call brems
# The following macro template allows the user to change the particle
# selection scheme (e.g., adding importance sampling such as splitting,
# leading particle selection, etc.).  (default macro is template
# '$ PARTICLE-SELECTION-ELECTR' which in turn has the 'null' replacement ';') 


IARG=BREMAUSA ;  IF (IAUSFL(IARG+1).NE.0) [CALL AUSGAB(IARG);]
if iq[np] == 0:

    # Photon was selected.
    return
    # i.e., return to shower
else:

    # Electron was selected
    go to :NEWELECTRON:

# ---------------------------------------------
# Electron cutoff energy discard section
# ---------------------------------------------
:ECUT-DISCARD:
if  medium > 0 :

    if eie > ae(medium):

        idr = EGSCUTAUS
        if lelec < 0:
            edep = e[np] - prm ELSE[EDEP=PEIE-PRM;]
    else:
         idr = PEGSCUTAUS; edep = e[np] - prm; 
else:
    idr = EGSCUTAUS; edep = e[np] - prm; 


;IARG=idr ;  IF (IAUSFL(IARG+1).NE.0) [CALL AUSGAB(IARG);]  # The default replacement for this macros is 
                     #           $ AUSCALL(idr)                    
                     # Use this macro if you wish to modify the   
                     # treatment of track ends                    

:POSITRON-ANNIHILATION: # NRCC extension 86/9/12

if lelec > 0:

    # It's a positron. Produce annihilation gammas if edep < peie
    if edep < peie:

IARG=ANNIHRAUSB ;  IF (IAUSFL(IARG+1).NE.0) [CALL AUSGAB(IARG);]
        call annih_at_rest


IARG=ANNIHRAUSA ;  IF (IAUSFL(IARG+1).NE.0) [CALL AUSGAB(IARG);]
        # Now discard the positron and take normal return to follow
        # the annihilation gammas.
        return # i.e., return to shower

] # end of positron block

np = np - 1
ircode = 2 # tell shower an e- or un-annihilated
            # e+ has been discarded

return # i.e., return to shower

# ---------------------------------------------
# User requested electron discard section
# ---------------------------------------------
:USER-ELECTRON-DISCARD:

idisc = abs(idisc)

if (lelec < 0) or (idisc == 99):

    edep = e[np] - prm
else:
    edep = e[np] + prm;

IARG=USERDAUS ;  IF (IAUSFL(IARG+1).NE.0) [CALL AUSGAB(IARG);]

if idisc == 99:

     goto :POSITRON-ANNIHILATION:

np = np - 1; ircode = 2

return # i.e., return to shower
end # End of subroutine electr
# *******************************************************************************