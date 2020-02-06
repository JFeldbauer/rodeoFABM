module tuddhyb_model_library

   use fabm_types, only: type_base_model_factory,type_base_model

   implicit none

   private

   type,extends(type_base_model_factory) :: type_factory
      contains
      procedure :: create
   end type

   type (type_factory),save,target,public :: tuddhyb_model_factory

contains

   subroutine create(self,name,model)
      
      use tuddhyb_rodeo
      ! Add new tuddhyb models here

      class (type_factory),intent(in) :: self
      character(*),        intent(in) :: name
      class (type_base_model),pointer :: model

      select case (name)
	 case ('rodeo'); allocate(type_tuddhyb_rodeo::model)
         ! Add case statements for new models here
      end select

   end subroutine create

end module
