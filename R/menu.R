inform_startup <- function()
{
  msg <- c(paste0(
    "

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

    version ",
    packageVersion("DCM")),
    "\nType 'citation(\"DCM\")' for citing this R package in publications.

    ")
  return(msg)
}


.onAttach <- function(lib, pkg)
{

  msg <- inform_startup()
  packageStartupMessage(msg)
  invisible()
}


