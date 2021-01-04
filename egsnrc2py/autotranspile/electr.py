def start_new_particle():
    medium = med[irl]


def call_user_electron():
    pass


def select_electron_mfp():
    RNNE1 = randomset()
    if RNNE1 == 0.0):
        RNNE1 = 1.E-30
    DEMFP = max([-log(RNNE1), EPSEMFP])

def AUSGAB(IARG):
    pass


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


if IMPLICIT_NONE:
    IMPLICIT_NONE()

IRCODE: int

$COMIN_ELECTR # default replacement produces the following:
               # COMIN/DEBUG,BOUNDS,EGS-VARIANCE-REDUCTION, ELECIN,EPCONT,
                        # ET-Control,MEDIA,MISC,STACK,THRESH,UPHIIN,
                        # UPHIOT,USEFUL,USER,RANDOM/
;COMIN/EII-DATA/
;COMIN/EM/
lambda_max: float
sigratio: float
u_tmp: float
v_tmp: float
w_tmp: float
random_tustep: bool

# $DEFINE_LOCAL_VARIABLES_ELECTR XXX do we need to type these?
# /******* trying to save evaluation of range.
do_range: bool
the_range: float
# */

# data ierust/0/ # To count negative ustep's

# save ierust


if CALL_USER_ELECTRON:
    CALL_USER_ELECTRON()

ircode = 1 # Set up normal return-which means there is a photon
            # with less available energy than the lowest energy electron,
            # so return to shower so it can call photon to follow it.
            # (For efficiency's sake, we like to stay in this routine
            #  as long as there are electrons to process. That's why this
            #  apparently convoluted scheme of STACK contro is effected.)

irold = p.ir # Initialize previous region
                   # (ir() is an integer that is attached to the particle's
                   #  phase space. It contains the region
                   #  number that the current particle is in.
                   #  Np is the stack pointer, it points to where on the
                   #  stack the current particle is.)
irl    = irold # region number in local variable



if start_new_particle:
    start_new_particle()
#  Default replacement for the above is medium = med(irl) 
#  This is made a macro so that it can be replaced with a call to a 
#  user provided function start_new_particle(); for the C/C++ interface 

while True:  # :NEWELECTRON: LOOP

    # Go once through this loop for each 'new' electron whose charge and
    # energy has not been checked

    lelec = p.iq # Save charge in local variable
                    # (iq = -1 for electrons, 0 for photons and 1 for positrons)
    qel   = (1+lelec)/2 #  = 0 for electrons, = 1 for positrons 
    peie  = p.e # precise energy of incident electron (double precision)
    eie   = peie # energy incident electron (conversion to single)

    if eie <= ecut(irl):

        go to :ECUT-DISCARD:
        # (Ecut is the lower transport threshold.)

    # medium = med(irl) # (This renders the above assignment redundant!)
    # The above assignment is unnecessary, IK, June 2003

    if p.wt == 0.0:

        go to :USER-ELECTRON-DISCARD: # added May 01

    while True:  # :TSTEP: LOOP

        # Go through this loop each time we recompute distance to an interaction
        /******* trying to save evaluation of range.
        do_range = True # compute the range in $COMPUTE_RANGE below
        ********/
        compute_tstep = True # MFP resampled => calculate distance to the
                                # interaction in the USTEP loop
        eke = eie - rm # moved here so that kinetic energy will be known
                        # to user even for a vacuum step, IK January 2000
        if medium != 0:

            # Not vacuum. Must sample to see how far to next interaction.


            if SELECT_ELECTRON_MFP:
                SELECT_ELECTRON_MFP()
                #  Default FOR $SELECT_ELECTRON_MFP; is: $RANDOMSET rnne1
                #                                        demfp = -log(rnne1)
                # ($RANDOMSET is a macro'ed random number generator)
                # (demfp = differential electron mean free path)

            elke = log(eke)
            # (eke = kinetic energy, rm = rest mass, all in units of MeV)
            $SET INTERVAL elke,eke # Prepare to approximate cross section


            if EVALUATE_SIG0:
                EVALUATE_SIG0()
               # The fix up of the fictitious method uses cross section per
               # energy loss. Therefore, demfp/sig is sub-threshold energy loss
               # until the next discrete interaction occures (see below)
               # As this quantity is a single constant for a material,
               # $SET INTERVAL is not necessary at this point. However, to not
               # completely alter the logic of the TSTEP and USTEP loops,
               # this is left for now

        ] # end non-vacuum test

        while True:  # :USTEP: LOOP

            # Here for each check with user geometry.
            # Compute size of maximum acceptable step, which is limited
            # by multiple scattering or other approximations.
            if medium == 0:

                    # vacuum

                    if EMFIELD_INITIATE_SET_TUSTEP:
                        EMFIELD_INITIATE_SET_TUSTEP()
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

                    if SET_TUSTEP_EM_FIELD:
                        SET_TUSTEP_EM_FIELD()
                    ustep = tustep
            else:

                # non-vacuum

                if SET_RHOF:
                    SET_RHOF()    # density ratio scaling template
                              # EGS allows the density to vary
                              # continuously (user option)


                if SCALE_SIG0:
                    SCALE_SIG0()
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

                    if CALCULATE_TSTEP_FROM_DEMFP:
                        CALCULATE_TSTEP_FROM_DEMFP()
                ] # end sig if-else

                # calculate stopping power
                if lelec < 0:
                    $EVALUATE dedx0 USING ededx(elke) # e-
                else:
                    $EVALUATE dedx0 USING pdedx(elke); # e+
                dedx  = rhof*dedx0

                # Determine maximum step-size (Formerly $SET_TUSTEP)
                $EVALUATE tmxs USING tmxs(elke)
                tmxs = tmxs/rhof

                # Compute the range to E_min(medium) (e_min is the first
                # energy in the table). Do not go more than range.
                # Don't replace this macro and don't override range, because
                # the energy loss evaluation below relies on the accurate
                # (and self-consistent) evaluation of range!

                if COMPUTE_RANGE:
                    COMPUTE_RANGE()

                # The RANDOMIZE-TUSTEP option as coded by AFB forced the
                # electrons to approach discrete events (Moller,brems etc.)
                # only in a single scattering mode => waste of CPU time.
                # Moved here and changed by IK Oct 22 1997
                random_tustep = $RANDOMIZE_TUSTEP
                if random_tustep:

                 rnnotu = randomset()
                    tmxs = rnnotu*min(tmxs,smaxir(irl))
                else:

                    tmxs = min(tmxs,smaxir(irl))

                tustep = min(tstep,tmxs,range)

                if SET_TUSTEP_EM_FIELD:
                    SET_TUSTEP_EM_FIELD() # optional tustep restriction in EM field


                if CALL_HOWNEAR:
                    CALL_HOWNEAR(tperp)
                p.dnear = tperp

                if RANGE_DISCARD:
                    RANGE_DISCARD()       # optional regional range rejection for
                                      # particles below e_max_rr if i_do_rr set


                if USER_RANGE_DISCARD:
                    USER_RANGE_DISCARD()  # default is ;, but user may implement


                if SET_SKINDEPTH:
                    SET_SKINDEPTH(eke,elke)
                  # This macro sets the minimum step size for a condensed
                  # history (CH) step. When the exact BCA is used, the minimum
                  # CH step is determined by efficiency considerations only
                  # At about 3 elastic MFP's single scattering becomes more
                  # efficient than CH and so the algorithm switches off CH
                  # If one of the various inexact BCA's is invoked, this macro
                  # provides a simple way to include more sophisticated
                  # decisions about the maximum acceptable approximated CH step

                tustep = min(tustep,max(tperp,skindepth))

                if EMFIELD_INITIATE_SET_TUSTEP:
                    EMFIELD_INITIATE_SET_TUSTEP()
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

                    if COMPUTE_ELOSS_G:
                        COMPUTE_ELOSS_G(tustep,eke,elke,lelke,de)

                    tvstep = tustep; is_ch_step = True

                    if transport_algorithm == $PRESTA_II:

                      call msdist_pII
                      (
                        # Inputs
                        eke,de,tustep,rhof,medium,qel,spin_effects,
                        p.u,p.v,p.w,p.x,p.y,p.z,
                        # Outputs
                        uscat,vscat,wscat,xtrans,ytrans,ztrans,ustep
                      )
                    else:

                      call msdist_pI
                      (
                        # Inputs
                        eke,de,tustep,rhof,medium,qel,spin_effects,
                        p.u,p.v,p.w,p.x,p.y,p.z,
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
                     rnnoss = randomset()
                        if  rnnoss < 1.e-30 :

                            rnnoss = 1.e-30

                        lambda = - Log(1 - rnnoss)
                        lambda_max = 0.5*blccl*rm/dedx*(eke/rm+1)**3
                        if  lambda >= 0 and lambda_max > 0 :

                            if  lambda < lambda_max :

                                tuss=lambda*ssmfp*(1-0.5*lambda/lambda_max)
                            else:
                              tuss = 0.5 * lambda * ssmfp

                            if tuss < tustep:

                                tustep = tuss
                                dosingle = True
                            else:
                                dosingle = False

                        else:
                          $egs_warning(*,' lambda > lambda_max: ',
                             lambda,lambda_max,' eke dedx: ',eke,dedx,
                             ' ir medium blcc: ',p.ir,medium,blcc(medium),
                             ' position = ',p.x,p.y,p.z)
                          dosingle = False
                          p.exists = False return

                        ustep = tustep
                    else:

                        # Boundary crossing a la EGS4/PRESTA-I but using
                        # exact PLC
                        dosingle = False
                        domultiple = True

                        if SET_USTEP:
                            SET_USTEP()

                    if ustep < tperp:

                        callhowfar = False
                    else:

                        callhowfar = True


            ] # end non-vacuum test


            if SET_USTEP_EM_FIELD:
                SET_USTEP_EM_FIELD()  # additional ustep restriction in em field
                                  # default for $SET_USTEP_EM_FIELD; is ;(null)
            irold  = p.ir # save current region
            irnew  = p.ir # default new region is current region
            idisc  = 0 # default is no discard (this flag is initialized here)
            ustep0 = ustep # Save the intended ustep.

            # IF(callhowfar) [ call howfar; ]

            if CALL_HOWFAR_IN_ELECTR:
                CALL_HOWFAR_IN_ELECTR() # The above is the default replacement

            # Now see if user requested discard
            if idisc > 0) # (idisc is returned by howfar:

                # User requested immediate discard
                go to :USER-ELECTRON-DISCARD:

            if CHECK_NEGATIVE_USTEP:
                CHECK_NEGATIVE_USTEP()

            if ustep == 0 or medium = 0:

                # Do fast step in vacuum
                if ustep != 0:

                    IF $EM_MACROS_ACTIVE

                        edep = pzero # no energy loss in vacuum
                        # transport in EMF in vacuum:
                        # only a B or and E field can be active
                        # (not both at the same time)

                        if EMFieldInVacuum:
                            EMFieldInVacuum()
                    else:

                        # Step in vacuum
                        vstep  = ustep
                        tvstep = vstep
                        # ( vstep is ustep truncated (possibly) by howfar
                        #  tvstep is the total curved path associated with vstep)
                        edep = pzero # no energy loss in vacuum

                        if VACUUM_ADD_WORK_EM_FIELD:
                            VACUUM_ADD_WORK_EM_FIELD()
                            # additional vacuum transport in em field
                        e_range = vacdst

                        if AUSCALL:
                            AUSCALL($TRANAUSB)
                        # Transport the particle
                        p.x = p.x + p.u*vstep
                        p.y = p.y + p.v*vstep
                        p.z = p.z + p.w*vstep
                        p.dnear = p.dnear - vstep
                            # (dnear is distance to the nearest boundary
                            #  that goes along with particle stack and
                            #  which the user's howfar can supply (option)

                        if SET_ANGLES_EM_FIELD:
                            SET_ANGLES_EM_FIELD()
                            # default for $SET_ANGLES_EM_FIELD; is ; (null)
                             # (allows for EM field deflection
                    ] # end of EM_MACROS_ACTIVE block
                ] # end of vacuum step

                if irnew != irold:

                     $electron_region_change; 

                if ustep != 0:

                    IARG = $TRANAUSA
                    if IAUSFL[IARG + 1] != 0:
                        AUSGAB(IARG)
                if eie <= ecut(irl):
                    go to :ECUT-DISCARD:
                if ustep != 0 and idisc < 0:
                    go to :USER-ELECTRON-DISCARD:
                NEXT :TSTEP:  # (Start again at :TSTEP:)

            ] # Go try another big step in (possibly) new medium

            vstep = ustep

            if EM_FIELD_SS:
                EM_FIELD_SS()
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

                    if SET_TVSTEP:
                        SET_TVSTEP()

                # Fourth order technique for dedx
                # Must be done for an approx. CH step or a
                # single scattering step.

                if COMPUTE_ELOSS_G:
                    COMPUTE_ELOSS_G(tvstep,eke,elke,lelke,de)
            else:

               # callhowfar=False => step has not been reduced due to
               #                       boundaries
               tvstep = tustep
               if  ~callmsdist :

                  # Second order technique for dedx
                  # Already done in a normal CH step with call to msdist

                  if COMPUTE_ELOSS_G:
                      COMPUTE_ELOSS_G(tvstep,eke,elke,lelke,de)


            if SET_TVSTEP_EM_FIELD:
                SET_TVSTEP_EM_FIELD() # additional path length correction in em field
                # ( Calculates tvstep given vstep
                #  default for $SET_TVSTEP_EM_FIELD; is ; (null)

            save_de = de # the energy loss is used to calculate the number
                              # of MFP gone up to now. If energy loss
                              # fluctuations are implemented, de will be
                              # changed in $DE_FLUCTUATION; => save

            # The following macro template allows the user to change the
            # ionization loss.
            # (Provides a user hook for Landau/Vavilov processes)

            if DE_FLUCTUATION:
                DE_FLUCTUATION()
                # default for $DE_FLUCTUATION; is ; (null)
            edep = de # energy deposition variable for user

            if ADD_WORK_EM_FIELD:
                ADD_WORK_EM_FIELD()  # e-loss or gain in em field

            if ADD_WORK_EM_FIELD:
                ADD_WORK_EM_FIELD()  # EEMF implementation
                # Default for $ADD_WORK_EM_FIELD; is ; (null)
            ekef = eke - de # (final kinetic energy)
            eold = eie # save old value
            enew = eold - de # energy at end of transport

            # Now do multiple scattering
            if  ~callmsdist :
                   # everything done if callmsdist  is True

                if  domultiple :

                    # Approximated CH step => do multiple scattering
                    # 
                    # ekems, elkems, beta2 have been set in either $SET_TUSTEP
                    # or $SET_TVSTEP if spin_effects is True, they are
                    # not needed if spin_effects is False
                    # 
                    # chia2,etap,xi,xi_corr are also set in the above macros
                    # 
                    # qel (0 for e-, 1 for e+) and medium are now also required
                    # (for the spin rejection loop)
                    # 
                    lambda = blccl*tvstep/beta2/etap/(1+chia2)
                    xi = xi/xi_corr
                    findindex = True; spin_index = True
                    call mscat(lambda,chia2,xi,elkems,beta2,qel,medium,
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
                         $SET INTERVAL elkems,eke
                         if lelec < 0:
                             $EVALUATE etap USING etae_ms(elkems)
                         else:
                             $EVALUATE etap USING etap_ms(elkems);
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

                IF ~($EM_MACROS_ACTIVE)

                    x_final = p.x + p.u*vstep
                    y_final = p.y + p.v*vstep
                    z_final = p.z + p.w*vstep

                if  domultiple or dosingle :

                    u_tmp = p.u; v_tmp = p.v; w_tmp = p.w
                    call uphi(2,1) # Apply the deflection, save call to uphi if
                                    # no deflection in a single scattering mode
                    u_final = p.u; v_final = p.v; w_final = p.w
                    p.u = u_tmp; p.v = v_tmp; p.w = w_tmp
                else:
                     u_final = p.u; v_final = p.v; w_final = p.w; 

            if AUSCALL:
                AUSCALL($TRANAUSB)

            # Transport the particle

            p.x = x_final; p.y = y_final; p.z = z_final
            p.u = u_final; p.v = v_final; p.w = w_final

            p.dnear = p.dnear - vstep
            irold = p.ir # save previous region

            if SET_ANGLES_EM_FIELD:
                SET_ANGLES_EM_FIELD()
            # Default for $SET_ANGLES_EM_FIELD; is ; (null)


            # Now done with multiple scattering,
            # update energy and see if below cut
            # below subtracts only energy deposited
            peie  = peie - edep
            # below subtracts energy deposited + work due to E field
            # peie = peie - de
            eie   = peie
            p.e = peie

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
                $SET INTERVAL elke,eke # Get updated interval

            if irnew != irold:

                 $electron_region_change; 

            # After transport call to user scoring routine

            if AUSCALL:
                AUSCALL($TRANAUSA)

            if eie <= ecut(irl):

               go to :ECUT-DISCARD:

            # Now check for deferred discard request.  May have been set
            # by either howfar, or one of the transport ausgab calls
            if idisc < 0:

              go to :USER-ELECTRON-DISCARD:

            if medium != medold:

                 NEXT :TSTEP:


            if USER_CONTROLS_TSTEP_RECURSION:
                USER_CONTROLS_TSTEP_RECURSION()
                # NRCC update 87/12/08--default is null


            if UPDATE_DEMFP:
                UPDATE_DEMFP()

        if demfp < $EPSEMFP:

            break  # end ustep loop

        # Compute final sigma to see if resample is needed.
        # this will take the energy variation of the sigma into
        # account using the fictitious sigma method.


        if EVALUATE_SIGF:
            EVALUATE_SIGF()

        sigratio = sigf/sig0

     rfict = randomset()

    if rfict <= sigratio:

        break   # end tstep loop

    #  Now sample electron interaction

    if lelec < 0:

        # e-,check branching ratio

        if EVALUATE_EBREM_FRACTION:
            EVALUATE_EBREM_FRACTION()
          # Default is $EVALUATE ebr1 USING ebr1(elke)
     rnno24 = randomset()
        if rnno24 <= ebr1:

            # It was bremsstrahlung
            go to :EBREMS:
        else:

            # It was Moller, but first check the kinematics.
            # However, if EII is on, we should still permit an interaction
            # even if E<moller threshold as EII interactions go down to
            # the ionization threshold which may be less than thmoll.
            if p.e <= thmoll(medium) and eii_flag == 0:
                
                 # (thmoll = lower Moller threshold)

                # Not enough energy for Moller, so
                # force it to be a bremsstrahlung---provided ok kinematically.
                if ebr1 <= 0:
                    go to :NEWELECTRON:
                    # Brems not allowed either.
                go to :EBREMS:

            if AUSCALL:
                AUSCALL($MOLLAUSB)
            call moller
            # The following macro template allows the user to change the
            # particle selection scheme (e.g., adding importance sampling
            # such as splitting, leading particle selection, etc.).
            # (Default macro is template '$PARTICLE_SELECTION_ELECTR'
            # which in turn has the 'null' replacement ';')

            if PARTICLE_SELECTION_MOLLER:
                PARTICLE_SELECTION_MOLLER()

            if AUSCALL:
                AUSCALL($MOLLAUSA)
            if  p.iq == 0 :
                 return

        go to :NEWELECTRON: # Electron is lowest energy-follow it

    # e+ interaction. pbr1 = brems/(brems + bhabha + annih

    if EVALUATE_PBREM_FRACTION:
        EVALUATE_PBREM_FRACTION()
       # Default is $EVALUATE pbr1 USING pbr1(elke)
 rnno25 = randomset()
    if rnno25 < pbr1:
        go to :EBREMS: # It was bremsstrahlung
    # Decide between bhabha and annihilation
    # pbr2 is (brems + bhabha)/(brems + bhabha + annih)

    if EVALUATE_BHABHA_FRACTION:
        EVALUATE_BHABHA_FRACTION()
       # Default is $EVALUATE pbr2 USING pbr2(elke)
    if rnno25 < pbr2:

        # It is bhabha

        if AUSCALL:
            AUSCALL($BHABAUSB)
        call bhabha
        # The following macro template allows the user to change the
        # particle selection scheme (e.g., adding importance sampling
        # such as splitting, leading particle selection, etc.).  (default
        # macro is template '$PARTICLE_SELECTION_ELECTR' which in turn
        # has the 'null' replacement ';')

        if PARTICLE_SELECTION_BHABHA:
            PARTICLE_SELECTION_BHABHA()

        if AUSCALL:
            AUSCALL($BHABAUSA)
        if  p.iq == 0 :
             return
    else:

        # It is in-flight annihilation

        if AUSCALL:
            AUSCALL($ANNIHFAUSB)
        call annih
        # The following macro template allows the user to change the
        # particle selection scheme (e.g., adding importance sampling
        # such as splitting, leading particle selection, etc.).  (default
        # macro is template '$PARTICLE_SELECTION_ELECTR' which in turn
        # has the 'null' replacement ';')

        if PARTICLE_SELECTION_ANNIH:
            PARTICLE_SELECTION_ANNIH()

        if AUSCALL:
            AUSCALL($ANNIHFAUSA)
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

if AUSCALL:
    AUSCALL($BREMAUSB)
call brems
# The following macro template allows the user to change the particle
# selection scheme (e.g., adding importance sampling such as splitting,
# leading particle selection, etc.).  (default macro is template
# '$PARTICLE_SELECTION_ELECTR' which in turn has the 'null' replacement ';')

if PARTICLE_SELECTION_BREMS:
    PARTICLE_SELECTION_BREMS()

if AUSCALL:
    AUSCALL($BREMAUSA)
if p.iq == 0:

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

        idr = $EGSCUTAUS
        if lelec < 0:
            edep = p.e - prm ELSE[$POSITRON_ECUT_DISCARD;]
    else:
         idr = $PEGSCUTAUS; edep = p.e - prm; 
else:
    idr = $EGSCUTAUS; edep = p.e - prm; 



if ELECTRON_TRACK_END:
    ELECTRON_TRACK_END() # The default replacement for this macros is 
                     #           $AUSCALL(idr)                   
                     # Use this macro if you wish to modify the   
                     # treatment of track ends                    

:POSITRON-ANNIHILATION: # NRCC extension 86/9/12

if lelec > 0:

    # It's a positron. Produce annihilation gammas if edep < peie
    if edep < peie:

        if AUSCALL:
            AUSCALL($ANNIHRAUSB)
        call annih_at_rest

        if PARTICLE_SELECTION_ANNIHREST:
            PARTICLE_SELECTION_ANNIHREST()

        if AUSCALL:
            AUSCALL($ANNIHRAUSA)
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

    edep = p.e - prm
else:
    edep = p.e + prm;


if AUSCALL:
    AUSCALL($USERDAUS)

if idisc == 99:

     goto :POSITRON-ANNIHILATION:

p.exists = False ircode = 2

return # i.e., return to shower
end # End of subroutine electr
# *******************************************************************************