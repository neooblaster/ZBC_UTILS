*&----------------------------------------------------------------------------&*
*&----------------------------------------------------------------------------&*
*&                                                                            &*
*&               Report ZBC_FLPCM_EXTRACT                                     &*
*&                                                                            &*
*&----------------------------------------------------------------------------&*
*&----------------------------[   MAIN INOS   ]-------------------------------&*
*&----------------------------------------------------------------------------&*
*&                                                                            &*
*&                                                                            &*
*&  Author      : Nicolas DUPRE (NDU90045)                                    &*
*&  Release     : xx.xx.2020                                                  &*
*&                                                                            &*
*&                                                                            &*
*&----------------------------------------------------------------------------&*
*&---------------------------[   DESCRIPTION   ]------------------------------&*
*&----------------------------------------------------------------------------&*
*&                                                                            &*
*&  The report must be less intrusive and the easiest to install as possible. &*
*&  The entire code stand in this only report and all text as well.           &*
*&  So this report contains hardcoded text.                                   &*
*&                                                                            &*
*&  The purpose of the report is to extract all FLP Catalogs with             &*
*&  their tile/target mapping and associated roles in an CSV Excel File       &*
*&                                                                            &*
*&                                                                            &*
*&                                                                            &*
*&----------------------------------------------------------------------------&*
*&----------------------------[   REVISIONS   ]-------------------------------&*
*&----------------------------------------------------------------------------&*
*&                                                                            &*
*&---------------#------------------------------------------------------------&*
*& Date / Author | Updates Descriptions                                       &*
*&---------------#------------------------------------------------------------&*
*&               |                                                            &*
*&---------------#------------------------------------------------------------&*
*&               |                                                            &*
*&---------------#------------------------------------------------------------&*
*&----------------------------------------------------------------------------&*
REPORT zbc_flpcm_extract.


INCLUDE zbc_flpcm_extract_top .
INCLUDE zbc_flpcm_extract_scr .
INCLUDE zbc_flpcm_extract_f01 .



*&----------------------------------------------------------------------------&"
*&   Initialization                                                           &"
*&----------------------------------------------------------------------------&"
INITIALIZATION.

  " Initialize Report
  PERFORM initialize.



*&----------------------------------------------------------------------------&"
*&   Start of processing                                                      &"
*&----------------------------------------------------------------------------&"
START-OF-SELECTION.

  " Get data
  PERFORM get_data .

  " Generate CSV Files
  PERFORM export_data_to_csv .




*&----------------------------------------------------------------------------&"
*&   End of processing                                                        &"
*&----------------------------------------------------------------------------&"









*&----------------------------------------------------------------------------&"
*&   Notes                                                                    &"
*&----------------------------------------------------------------------------&"
" Methods of class /ui2/if_flp_cont_mgr :
*
*
*   ADD_ROLE_CATALOG_ASSIGNMENT
*   ADD_TILES_TMS_TO_CATALOG
*   ADD_TO_ROLE
*   CHECK_SERVICES
*   CHECK_TECH_PREREQUISITES
*   COPY_CATALOG
*   CREATE_CATALOG
*   DELETE_CATALOG
*   GET_ALL_CATALOGS                                      <<<<<<
*   GET_ALL_ROLES                                         <<<<<<
*   GET_ALL_TILES_TMS                                         x
*   GET_CATALOG                                               x
*   GET_CATALOGS_OF_ROLES                                     x
*   GET_CATALOGS_OF_TILE_TM                                   x
*   GET_GROUP
*   GET_GROUPS_OF_ROLE
*   GET_ICF_SERVICES
*   GET_LAST_SERVICE_CHECK_RESULT
*   GET_MESSAGES_OF_TILE_TM                                   x
*   GET_ODATA_SERVICES
*   GET_ROLES_OF_CATALOG                                      x
*   GET_ROLES_OF_TILE_TM                                      x
*   GET_SPACES_OF_ROLE                                        x
*   GET_SUCCESSOR_TCODES_FOR_TCODE
*   GET_TILES_TMS_OF_CATALOG                              <<<<<<
*   GET_TILES_TMS_OF_ROLE                                     x
*   INITIALIZE_TABLES_FOR_CATALOGS                        <<<<<<
*   INITIALIZE_TABLES_FOR_ROLES                           <<<<<<
*   IS_SERVICE_CHECK_ALLOWED
*   REMOVE_CATALOGS_FROM_ROLES
*   REMOVE_FROM_ROLE
*   REMOVE_ROLE_CATALOG_ASSIGNMENT
*   REMOVE_TILES_TMS_FROM_CATALOG
*   RENAME_CATALOG
*   SERVICE_CHECK_ALL
*   SERVICE_CHECK_FOR_CATALOGS
*   SERVICE_CHECK_FOR_ROLES
*   SERVICE_CHECK_FOR_TTMS
*   TRANSPORT_CATALOG
*   UPDATE_ROLE_STATUS
*
*
*
