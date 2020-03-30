MODULE Shared
    use KindType
    implicit none
    save
    !
    type(FILE_LIST)      :: MY_FILES
    type(ERROR_STATUS)   :: MY_ERR
    type(CONFIG_PARAMS)  :: MY_CONFIG
    type(SIR)            :: MY_MODEL
!    type(MODEL_OUTPUT)   :: MY_OUT
    !
END MODULE Shared
