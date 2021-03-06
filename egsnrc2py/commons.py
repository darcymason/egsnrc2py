from egsnrc2py import egsfortran

#  COMMON block randomm --------
randomm = egsfortran.randomm
rng_seed = randomm.rng_seed
rng_array = randomm.rng_array
seeds = randomm.seeds

#  COMMON block stack --------
stack = egsfortran.stack
e = stack.e
x = stack.x
y = stack.y
z = stack.z
u = stack.u
v = stack.v
w = stack.w
dnear = stack.dnear
wt = stack.wt
iq = stack.iq
ir = stack.ir
latch = stack.latch
latchi = stack.latchi
np = stack.np
npold = stack.npold

#  COMMON block media --------
media_ = egsfortran.media
rlc = media_.rlc
rldu = media_.rldu
rho = media_.rho
msge = media_.msge
mge = media_.mge
mseke = media_.mseke
meke = media_.meke
mleke = media_.mleke
mcmfp = media_.mcmfp
mrange = media_.mrange
iraylm = media_.iraylm
iphotonucm = media_.iphotonucm
media = media_.media
iphotonuc = media_.iphotonuc
nmed = media_.nmed
eii_xfile = media_.eii_xfile
photon_xsections = media_.photon_xsections
comp_xsections = media_.comp_xsections
photonuc_xsections = media_.photonuc_xsections

#  COMMON block bounds --------
bounds = egsfortran.bounds
ecut = bounds.ecut
pcut = bounds.pcut
vacdst = bounds.vacdst

#  COMMON block elecin --------
elecin = egsfortran.elecin
esige_max = elecin.esige_max
psige_max = elecin.psige_max
esig_e = elecin.esig_e
psig_e = elecin.psig_e
range_ep = elecin.range_ep
e_array = elecin.e_array
etae_ms0 = elecin.etae_ms0
etae_ms1 = elecin.etae_ms1
etap_ms0 = elecin.etap_ms0
etap_ms1 = elecin.etap_ms1
q1ce_ms0 = elecin.q1ce_ms0
q1ce_ms1 = elecin.q1ce_ms1
q1cp_ms0 = elecin.q1cp_ms0
q1cp_ms1 = elecin.q1cp_ms1
q2ce_ms0 = elecin.q2ce_ms0
q2ce_ms1 = elecin.q2ce_ms1
q2cp_ms0 = elecin.q2cp_ms0
q2cp_ms1 = elecin.q2cp_ms1
blcce0 = elecin.blcce0
blcce1 = elecin.blcce1
eke0 = elecin.eke0
eke1 = elecin.eke1
xr0 = elecin.xr0
teff0 = elecin.teff0
blcc = elecin.blcc
xcc = elecin.xcc
esig0 = elecin.esig0
esig1 = elecin.esig1
psig0 = elecin.psig0
psig1 = elecin.psig1
ededx0 = elecin.ededx0
ededx1 = elecin.ededx1
pdedx0 = elecin.pdedx0
pdedx1 = elecin.pdedx1
ebr10 = elecin.ebr10
ebr11 = elecin.ebr11
pbr10 = elecin.pbr10
pbr11 = elecin.pbr11
pbr20 = elecin.pbr20
pbr21 = elecin.pbr21
tmxs0 = elecin.tmxs0
tmxs1 = elecin.tmxs1
expeke1 = elecin.expeke1
iunrst = elecin.iunrst
epstfl = elecin.epstfl
iaprim = elecin.iaprim
sig_ismonotone = elecin.sig_ismonotone

#  COMMON block thresh --------
thresh = egsfortran.thresh
rmt2 = thresh.rmt2
rmsq = thresh.rmsq
ap = thresh.ap
ae = thresh.ae
up = thresh.up
ue = thresh.ue
te = thresh.te
thmoll = thresh.thmoll

#  COMMON block uphiot --------
uphiot = egsfortran.uphiot
theta = uphiot.theta
sinthe = uphiot.sinthe
costhe = uphiot.costhe
sinphi = uphiot.sinphi
cosphi = uphiot.cosphi
pi = uphiot.pi
twopi = uphiot.twopi
pi5d2 = uphiot.pi5d2

#  COMMON block useful --------
#  from tutor1.f: DATA RM,PRM,PRMT2,PZERO/0.5109989461,0.5109989461,1.0219978922,0.D0/
useful = egsfortran.useful
pzero = useful.pzero = 0.0
prm = useful.prm = 0.5109989461
prmt2 = useful.prmt2 = 1.0219978922
rm = useful.rm = 0.5109989461
medium = useful.medium
medold = useful.medold

#  COMMON block misc --------
misc = egsfortran.misc
dunit = misc.dunit
kmpi = misc.kmpi
kmpo = misc.kmpo
rhor = misc.rhor
med = misc.med
iraylr = misc.iraylr
iphotonucr = misc.iphotonucr

#  COMMON block epcont --------
epcont = egsfortran.epcont
edep = epcont.edep
edep_local = epcont.edep_local
tstep = epcont.tstep
tustep = epcont.tustep
ustep = epcont.ustep
tvstep = epcont.tvstep
vstep = epcont.vstep
rhof = epcont.rhof
eold = epcont.eold
enew = epcont.enew
eke = epcont.eke
elke = epcont.elke
gle = epcont.gle
e_range = epcont.e_range
x_final = epcont.x_final
y_final = epcont.y_final
z_final = epcont.z_final
u_final = epcont.u_final
v_final = epcont.v_final
w_final = epcont.w_final
idisc = epcont.idisc
irold = epcont.irold
irnew = epcont.irnew
iausfl = epcont.iausfl