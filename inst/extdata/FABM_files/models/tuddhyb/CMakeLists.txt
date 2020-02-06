add_library(fabm_models_tuddhyb OBJECT
            tuddhyb_model_library.F90
	    rodeo/rodeo.F90
           )

add_dependencies(fabm_models_tuddhyb fabm_base)
