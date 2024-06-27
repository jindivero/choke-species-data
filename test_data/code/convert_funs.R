

calc_sigma <- function(s, t, p) {
  constants <- list(B0 = 8.24493e-1, B1 = -4.0899e-3, B2 = 7.6438e-5, B3 = -8.2467e-7, B4 = 5.3875e-9, C0 = -5.72466e-3, C1 = 1.0227e-4, C2 = -1.6546e-6, D0 = 4.8314e-4, A0 = 999.842594, A1 = 6.793952e-2, A2 = -9.095290e-3, A3 = 1.001685e-4, A4 = -1.120083e-6, A5 = 6.536332e-9, FQ0 = 54.6746, FQ1 = -0.603459, FQ2 = 1.09987e-2, FQ3 = -6.1670e-5, G0 = 7.944e-2, G1 = 1.6483e-2, G2 = -5.3009e-4, i0 = 2.2838e-3, i1 = -1.0981e-5, i2 = -1.6078e-6, J0 =1.91075e-4, M0 = -9.9348e-7, M1 = 2.0816e-8, M2 = 9.1697e-10, E0 = 19652.21, E1 = 148.4206, E2 = -2.327105, E3 = 1.360477e-2, E4 = -5.155288e-5, H0 = 3.239908, H1 = 1.43713e-3, H2 = 1.16092e-4, H3 = -5.77905e-7, K0 = 8.50935e-5, K1 =-6.12293e-6, K2 = 5.2787e-8)
  
  list2env(constants, environment())
  constant_names <- names(constants)
  t2 = t*t
  t3 = t*t2
  t4 = t*t3
  t5 = t*t4
#  if (s <= 0.0) s = 0.000001
  s32 = s^ 1.5
  p = p / 10.0 # convert decibars to bars */
  sigma = A0 + A1*t + A2*t2 + A3*t3 + A4*t4 + A5*t5 + (B0 + B1*t + B2*t2 + B3*t3 + B4*t4)*s + (C0 + C1*t + C2*t2)*s32 + D0*s*s
  kw = E0 + E1*t + E2*t2 + E3*t3 + E4*t4
  aw = H0 + H1*t + H2*t2 + H3*t3
  bw = K0 + K1*t + K2*t2
  k = kw + (FQ0 + FQ1*t + FQ2*t2 + FQ3*t3)*s + (G0 + G1*t + G2*t2)*s32 + (aw + (i0 + i1*t + i2*t2)*s + (J0*s32))*p + (bw + (M0 + M1*t + M2*t2)*s)*p*p
  val = 1 - p / k
  sigma = sigma / val - 1000.0
  rm(constant_names)
  return(sigma)
}

calc_sigma(s = yearly_newport$practical.salinity[1], 
           t = yearly_newport$temperature..degC.[1],
           p = yearly_newport$pressure..dbar.[1])
# convert ml /l to umol / kg

convert_o2 <- function(o2ml_l, sigma) (o2ml_l * 44.660)/((1000 + sigma)/1000)
convert_o2(yearly_newport$dissolved.oxygen..ml.L.[1], calc_sigma(s = yearly_newport$practical.salinity[1], 
                                                                 t = yearly_newport$temperature..degC.[1],
                                                                 p = yearly_newport$pressure..dbar.[1])
)

yearly_newport$sigma <- calc_sigma( s = yearly_newport$practical.salinity,
           t = yearly_newport$temperature..degC.,
           p = yearly_newport$pressure..dbar.)
yearly_newport$o2 <- convert_o2(yearly_newport$dissolved.oxygen..ml.L., yearly_newport$sigma)
