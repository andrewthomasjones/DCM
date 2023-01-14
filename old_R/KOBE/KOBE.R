###################################################################################
#KKKKKKKKK    KKKKKKK     OOOOOOOOO     BBBBBBBBBBBBBBBBB   EEEEEEEEEEEEEEEEEEEEEE#
#K:::::::K    K:::::K   OO:::::::::OO   B::::::::::::::::B  E::::::::::::::::::::E#
#K:::::::K    K:::::K OO:::::::::::::OO B::::::BBBBBB:::::B E::::::::::::::::::::E#
#K:::::::K   K::::::KO:::::::OOO:::::::OBB:::::B     B:::::BEE::::::EEEEEEEEE::::E#
#KK::::::K  K:::::KKKO::::::O   O::::::O  B::::B     B:::::B  E:::::E       EEEEEE#
#K:::::K K:::::K   O:::::O     O:::::O  B::::B     B:::::B  E:::::E               #
#K::::::K:::::K    O:::::O     O:::::O  B::::BBBBBB:::::B   E::::::EEEEEEEEEE     #
#K:::::::::::K     O:::::O     O:::::O  B:::::::::::::BB    E:::::::::::::::E     #
#K:::::::::::K     O:::::O     O:::::O  B::::BBBBBB:::::B   E:::::::::::::::E     #
#K::::::K:::::K    O:::::O     O:::::O  B::::B     B:::::B  E::::::EEEEEEEEEE     #
#K:::::K K:::::K   O:::::O     O:::::O  B::::B     B:::::B  E:::::E               #
#KK::::::K  K:::::KKKO::::::O   O::::::O  B::::B     B:::::B  E:::::E       EEEEEE#
#K:::::::K   K::::::KO:::::::OOO:::::::OBB:::::BBBBBB::::::BEE::::::EEEEEEEE:::::E#
#K:::::::K    K:::::K OO:::::::::::::OO B:::::::::::::::::B E::::::::::::::::::::E#
#K:::::::K    K:::::K   OO:::::::::OO   B::::::::::::::::B  E::::::::::::::::::::E#
#KKKKKKKKK    KKKKKKK     OOOOOOOOO     BBBBBBBBBBBBBBBBB   EEEEEEEEEEEEEEEEEEEEEE#
###################################################################################

source('./R/KOBEdatain.R') # Step 1: Read in one or more datasets into KOBE.
source('./R/KOBEjoin.R')   # Step 2: Join two datasets together (Optional).
source('./R/KOBErun.R')    # Step 3: Run KOBE and follow the prompts
