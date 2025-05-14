module GHGCaseParam where
import Prob(Probability)

pS_Start, pD_Delay :: Probability
pS_Start = 0.9
pD_Delay = 0.9

pL_S_DH, pL_S_DL, pL_S_SL, pL_S_SH :: Probability
pL_S_DH = 0.7
pL_S_DL = 0.9
pL_S_SL = 0.7
pL_S_SH = 0.3

pU_S_0, pU_D_0 :: Probability
pU_S_0 = 0.9
pU_D_0 = 0.7

pU_S, pU_D :: Probability
pU_S = 0.9
pU_D = 0.3

-- Probabilities computed from assumptions

pL_D_DH, pL_D_DL :: Probability
pL_D_DH = pL_S_SH
pL_D_DL = pL_S_SL

-- Probabilities computed from "sum to 1.0" requirement
pD_Start, pS_Delay :: Probability
pD_Start = (1.0 - pS_Start)
pS_Delay = (1.0 - pD_Delay)

pH_S_DH, pH_S_SH, pH_S_DL, pH_S_SL :: Probability
pH_S_DH = (1.0 - pL_S_DH)
pH_S_SH = (1.0 - pL_S_SH)
pH_S_DL = (1.0 - pL_S_DL)
pH_S_SL = (1.0 - pL_S_SL)

pH_D_DH, pH_D_DL :: Probability
pH_D_DH = (1.0 - pL_D_DH)
pH_D_DL = (1.0 - pL_D_DL)

pC_S_0, pC_D_0, pC_S, pC_D :: Probability
pC_S_0 = (1.0 - pU_S_0)
pC_D_0 = (1.0 - pU_D_0)
pC_S = (1.0 - pU_S)
pC_D = (1.0 - pU_D)
