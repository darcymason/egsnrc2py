
# EMPTY CALLBACKS ----




#    OUTPUT {P1},KE,IQ({P1}),IR({P1}),X({P1}),Y({P1}),Z({P1}),U({P1}),V({P1}),
#    W({P1}),LATCH({P1}),WT({P1});  ({P2},I5,F9.3,2I4,3F8.3,3F7.3,I10,1PE10.3)


def WATCH(IARG,IWATCH):

# =============================================================================
#                                                                              
#      A general purpose auxiliary routine for use with the EGSnrc system
# 
#      It prints out information about the particle transport                  
#                                                                              
#        For IWATCH = 1 it prints information about each discrete interaction  
#        For IWATCH = 2 or 3 it prints information about each step as well     
#        For IWATCH = 4 it prints graphing data for use with EGS_Windows       
#                                                                              
#                                                                              
#     Routine is used via two mandatory and 1 optional call from the user's    
#           code                                                               
#                                                                              
#    1)The routine must be initialized by a call with IARG=-99 before the first
#           call to SHOWER. It should be after all inputs are in place.        
#    2)The routine must be called near the beginning of the AUSGAB subroutine  
#           IF (IWATCH > 0 ) CALL WATCH(IARG,IWATCH)                          
#    3)The routine may be called at the end of each history with IARG = - 1 so 
#           a message will get printed stated history is complete              
#                                                                              
#     Since WATCH cannot output values related to the initial values in a      
#     shower call, it is useful to also put something like the following       
#     immediately prior to the CALL SHOWER stmt                                
#            if((IWATCH != 0) and (IWATCH ~= 4))[                                
#               OUTPUT 1,EIN,IQI,IRI,XI,YI,ZI,UI,VI,WI,LATCHI,WTI              
#                (/' INITIAL SHOWER VALUES',T36,':',                           
#                I5,F9.3,2I4,3F8.3,3F7.3,I10,1PE10.3)                         
#            ]                                                                 
#     Note EIN is the kinetic energy of the incident particle                  
#                                                                              
#                                                                              
#    The routine uses up to 132 columns for output.                            
#                                                                              
#      JAN 1984  GENERALIZED VERSION WITH INITIALIZATION                       
#                               DAVE ROGERS NRCC                               
#      JUN 1987  PUT IN IWATCH = 4 OPTION     AFB                              
#      JUL 1988  COMPATIBLE WITH X-RAY FLUORESCENCE  DWOR                      
#      SEP 1990  ADDED ENERGY OUTPUT TO IWATCH = 4 OPTION     AFB              
#      OCT 1990  UNIX compatible carriage control   DWOR                       
#      JAN 2000  Rewritten to output relaxation particles and also             
#                so some of the output makes more sense BW                     
#                                                                              
# *****************************************************************************

# define a local macro


$INTEGER iarg,iwatch,IP,ICOUNT,JHSTRY,J,N
$REAL KE
$INTEGER graph_unit
integer  egs_open_file
$INTEGER ku,kr,ka

;COMIN/BOUNDS, STACK,EPCONT,EGS-VARIANCE-REDUCTION,USEFUL,EGS-IO/

DATA ICOUNT/0/,JHSTRY/1/ graph_unit/-1/
save     ICOUNT,JHSTRY,graph_unit

ku = 13; kr = 0; ka = 1
if IARG == -99:
     [ # Initialize flags so we will get calls thru AUSGAB
  DO J=1,29[IAUSFL(J)=1;]
  /IAUSFL(22),IAUSFL(23),IAUSFL(24)/=0

if IARG == -1:

     [# main is assumed to call AUSGAB with IARG=-1 at end of history
  if IWATCH == 4:

    if  graph_unit < 0 :

        graph_unit = egs_open_file(ku,kr,ka,'.egsgph')

    WRITE(graph_unit,:GRAPHICS_FORMAT:) 0,0,0,0.0,0.0,0.0,0.0,JHSTRY
    JHSTRY=JHSTRY+1
  else:
    OUTPUT JHSTRY;(' END OF HISTORY',I8,3X,40('*')/)
    JHSTRY=JHSTRY+1;ICOUNT=ICOUNT+2;RETURN


if  (IWATCH != 4) and ((ICOUNT >= 50) or (ICOUNT == 0) or (IARG = -99)) :

   # PRINT HEADER
   ICOUNT=1
   OUTPUT;(//T39,' NP',3X,'ENERGY  Q REGION    X',7X,
   'Y',7X,'Z',6X,'U',6X,'V',6X,'W',6X,'LATCH',2X,'WEIGHT'/)

if (IWATCH == 4) and (IARG >= 0) and (IARG != 5):

     [ # GRAPHICS OUTPUT
   if  graph_unit < 0 ) graph_unit == egs_open_file(ku,kr,ka,'.egsgph':
       
   WRITE(graph_unit,:GRAPHICS_FORMAT:) NP,iq[np],ir[np],x[np],y[np],z[np],e[np]
   :GRAPHICS_FORMAT:FORMAT(2I4,1X,I6,4G15.8,I12)

if IARG == 5  or  IARG < 0:

     RETURN
if IWATCH == 4:
     RETURN # NONE OF THE REST NEEDED FOR GRAPHICS OUTPUT

KE=e[np];if(iq[np] != 0)[KE=e[np]-PRM;]

if IARG == 0 and IWATCH = 2:

icount += 1
std_data(NP)

else:

    RETURN;
if     IARG == 1:

icount += 1
std_data(NP)
else:

icount += 1
std_data(NP)
else:

icount += 1
std_data(NP)
else:
  (T10,'Local energy deposition',T36,':',F12.5,' MeV in region ',I6);]

else:

icount += 1
std_data(NP)
else:
    if nbr_split =1:
         [# no splitting or SBS is on in BEAMnrc
       DO IP=NPold,NP[
         if IQ(IP)=-1:

           KE = E(IP) - RM

icount += 1
std_data(IP)

         else:
           KE = E(IP)

icount += 1
std_data(IP)


    else:
      KE = E(NPold) - RM

icount += 1
std_data(NPold)

      DO IP=NPold+1,NP[
        KE= E(IP)
        if IP == NPold+1:
            [# print info for first one only

icount += 1
std_data(IP)

        else:
icount += 1
std_data(IP)


    ]#  end of splitting block
else:
icount += 1
std_data(NP)
else:
  # surely this logic not needed?
  if NP=NPold:

icount += 1
std_data(NP)

  else:
     DO IP=NPold,NP[
        KE = E(IP) - ABS(iq[np])*RM
        if IP=NPold:

icount += 1
std_data(IP)
else:
icount += 1
std_data(IP)



else:

icount += 1
std_data(NP)

else:
  if NP=NPold:

icount += 1
std_data(NP)

  else:
     DO IP=NPold,NP[
        KE = E(IP) - ABS(IQ(IP))*RM
        if IP=NPold:

icount += 1
std_data(IP)
 else:
icount += 1
std_data(IP)



else:

icount += 1
std_data(NP)

else:
  if NP=NPold:

icount += 1
std_data(NP)

  else:
     DO IP=NPold,NP[
        KE = E(IP) - ABS(IQ(IP))*RM
        if IP=NPold:

icount += 1
std_data(IP)
else:
icount += 1
std_data(IP)



else:

icount += 1
std_data(NP)

else:
  if NP=NPold:

icount += 1
std_data(NP)

  else:
     DO IP=NPold,NP[
        KE = E(IP) - ABS(IQ(IP))*RM
        if IP=NPold:

icount += 1
std_data(IP)
else:
icount += 1
std_data(IP)



else:

icount += 1
std_data(NP)

else:
  if NP=NPold and i_survived_rr == 0:

icount += 1
std_data(NP)

  else:
      OUTPUT i_survived_rr,prob_rr
       (T10,'Russian Roulette eliminated ',I2,
                    ' particle(s) with probability ',F8.5)

icount += 1
std_data(NP)

  else:
     DO IP=NPold,NP[
        KE = E(IP) - ABS(IQ(IP))*RM
        if IP=NPold:

icount += 1
std_data(IP)
else:
icount += 1
std_data(IP)


     if i_survived_rr > 0:

        OUTPUT i_survived_rr,prob_rr
       (T10,'Russian Roulette eliminated ',I2,'
             particle(s) with probability ',F8.5)

icount += 1
std_data(NP)


else:
icount += 1
std_data(NP)
else:
  if NP == NPold and i_survived_rr = 0:

icount += 1
std_data(NP)

  else:
     DO IP=NPold,NPold+1[
        KE = E(IP) - ABS(IQ(IP))*RM
        if IQ(IP) != 0:

icount += 1
std_data(IP)
else:
icount += 1
std_data(IP)



  if i_survived_rr > 0:
      [# whether the stack has been cleared or not
       OUTPUT i_survived_rr,prob_rr
       (T10,'Russian Roulette eliminated ',I2,
                    ' particle(s) with probability ',F8.5)

icount += 1
std_data(NP)

else:

icount += 1
std_data(NP)

else:
    if NPold=NP and iq[np]=0 and i_survived_rr == 0:

      $CNTOUT(NP);(T11,'Photon energy below N-shell',/,
                   T11,'Photon discarded',T36,':')
    else:
      KE= E(NPold)-RM

icount += 1
std_data(NPold)

    else:
       if NP=NPold-1 or IQ(NPold) != -1:

          if i_survived_rr > 1:
              [# eliminated more than the photoelectron
              OUTPUT i_survived_rr-1,prob_rr
              (T10,'Russian Roulette eliminated ',I4,
                    ' particle(s) with probability ',F8.5,' plus')

          OUTPUT prob_rr
                 (T10,'Russian Roulette eliminated resulting photoelectron',
                      ' with probability ',F8.5)
       else:
           KE = E(NPold) - RM

icount += 1
std_data(NPold)

           OUTPUT i_survived_rr,prob_rr
           (T10,'Russian Roulette eliminated ',I4,
                    ' particle(s) with probability ',F8.5)

icount += 1
std_data(NP)

else:
icount += 1
std_data(NP)
else:
icount += 1
std_data(NP)
else:
icount += 1
std_data(NP)
else:
icount += 1
std_data(NP)

if IARG == 0  and  IWATCH = 2:

    [OUTPUT  USTEP,TUSTEP,VSTEP,TVSTEP,EDEP
(T5,'USTEP,TUSTEP,VSTEP,TVSTEP,EDEP',T36,':    ',5(1PE13.4));ICOUNT=ICOUNT+1;]

if NP == 1  or  IARG = 0:

     RETURN
if  IARG <= 3:

       N=NP-1
       KE = E(N) - ABS(IQ(N))*RM

icount += 1
std_data(N)

RETURN;END
