#include "fabm_driver.h"
module tuddhyb_rodeo
	use fabm_types
	implicit none
	private
	type, extends(type_base_model), public :: type_tuddhyb_rodeo
		

	contains

		! Reference model procedures here.
		procedure :: initialize
	

	end type


	contains


	subroutine initialize(self,configunit)

		class (type_tuddhyb_rodeo), intent(inout), target :: self

		integer, intent(in) :: configunit

		
	end subroutine initialize


	! Add model subroutines here.

	

end module
