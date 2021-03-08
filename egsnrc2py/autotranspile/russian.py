
# EMPTY CALLBACKS ----



# CALLBACKS ---- 

def randomset():
    global rng_seed

    if rng_seed > 24:
        ranlux(rng_array)
        rng_seed = 1

    random_num = rng_array[rng_seed-1]
    rng_seed += 1

    return random_num


# This macro implements Russian Roulette (most useful  with brems splitting)
# It is more efficient than having the user do it via AUSGAB since it avoids
# considerable handling of the particles by ELECTR
# The user must set i_play_RR (defaults to 0) and prob_RR
# Both are in COMIN EGS-VARIANCE-REDUCTION
# 
# Note that this macro is called as $ PLAY RUSSIAN ROULETTE WITH ELECTRONS... 
# Note also that subroutine pair has its own, internal version

subroutine Russian_Roulette

  i_survived_RR = 0 # flag all survive
  ;if( i_play_RR == 1 ) [
      if  prob_RR <= 0 :

          if  n_RR_warning < MAX_RR_WARNING :

            n_RR_warning = n_RR_warning + 1
            OUTPUT prob_RR
  ('**** Warning, attempt to play Roussian Roulette with prob_RR<=0! ',g14.6)

      else:
          ip = {P1}
          LOOP [     # handle all particles from p1 to np
              if  iq(ip) != 0 :
                   [   # i.e. charged particles
                  rnno_RR = randomset()
                  if  rnno_RR < prob_RR :
                       [ # particle survives
                      wt(ip) = wt(ip)/prob_RR
                      ip = ip + 1 # increase local pointer
                  else:
                      ;i_survived_RR = i_survived_RR + 1
                      ;IF(ip < np) [
                          # =>replace it with last particle on stack
                          e(ip) = e[np]; iq(ip) = iq[np]; wt(ip) = wt[np]
                          u(ip) = u[np]; v(ip) = v[np]; w(ip) = w[np]

                      np = np-1 # reduce stack by one=> particle gone
                  ] # end of kill particle block
              else:
                   # this is a photon, leave it. Change pointer ip = ip+1; 
          if ip > np:
              break 
          # loops until either np is decreased to ip, or ip increased to np
          if  np == 0 :
               [ #  we need at least one particle on the stack 
                         #  so that the transport routines can exit properly
              np = 1; e[np] = 0; iq[np] = 0; wt[np] = 0

      ] # end of russian roulette block
  ] # end of flag set block

