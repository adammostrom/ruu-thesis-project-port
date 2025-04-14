module Probabilities(..) where


pS_Start :: Double
pS_Start = 0.9

pD_Start :: Double
pD_Start = (1.0 - pS_Start)

pD_Delay :: Double
pD_Delay = 0.9

pS_Delay :: Double
pS_Delay = (1.0 - pD_Delay)

pL_S_DH :: Double
pL_S_DH = 0.7

pL_S_DL :: Double
pL_S_DL = 0.9

pL_S_SL :: Double
pL_S_SL = 0.7

pL_S_SH :: Double
pL_S_SH = 0.3

pL_D_DH :: Double
pL_D_DH = pL_S_SH

pL_D_DL :: Double
pL_D_DL = (pL_S_SL)

pU_S_0 :: Double
pU_S_0 = 0.9

pU_D_0 :: Double
pU_D_0 = 0.7

pU_S :: Double
pU_S = 0.9

pU_D :: Double
pU_D = 0.3

pH_S_DH :: Double
pH_S_DH = (1.0 - pL_S_DH)

pH_S_SH :: Double
pH_S_SH = (1.0 - pL_S_SH)

pH_S_DL :: Double
pH_S_DL = (1.0 - pL_S_DL)

pH_S_SL :: Double
pH_S_SL = (1.0 - pL_S_SL)

pH_D_DH :: Double
pH_D_DH = (1.0 - pL_D_DH)

pH_D_DL :: Double
pH_D_DL = (1.0 - pL_D_DL)

pC_S_0 :: Double
pC_S_0 = (1.0 - pU_S_0)

pC_D_0 :: Double
pC_D_0 = (1.0 - pU_D_0)

pC_S :: Double
pC_S = (1.0 - pU_S)

pC_D :: Double
pC_D = (1.0 - pU_D)
