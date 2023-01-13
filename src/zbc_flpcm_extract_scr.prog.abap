*&----------------------------------------------------------------------------&*
*&----------------------------------------------------------------------------&*
*&                                                                            &*
*&               Report ZBC_FLPCM_EXTRACT_SCR                                 &*
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
*&----------------------------------------------------------------------------&*=


*&----------------------------------------------------------------------------&"
*&   Selection Screen                                                         &"
*&----------------------------------------------------------------------------&"
PARAMETERS: p_scope TYPE /ui2/fdm_scope DEFAULT gc_scope_cust .



*&----------------------------------------------------------------------------&"
*&   Screen Validations                                                       &"
*&----------------------------------------------------------------------------&"
AT SELECTION-SCREEN OUTPUT.

  " Set Parameters Texts lables
  LOOP AT SCREEN.
    " Handle Parameter Text Label : %_xxxxxx_%_APP_%-TEXT
    IF screen-name CS gc_param_txt_label_var_name.
      " Count Screen Name Length
      gv_screen_name_len        = strlen( screen-name ).                              " Len of %_xxxxxx_%_APP_%-TEXT
      gv_screen_name_suffix_len = strlen( gc_param_txt_label_var_name ).              " Len of _%_APP_%-TEXT
      gv_param_name_len         = gv_screen_name_len - gv_screen_name_suffix_len - 2. " Len of xxxxxx  (-2 is for begenning %_)
      gv_param                  = screen-name+2(gv_param_name_len) .

      " Get text label
      PERFORM get_text USING gv_param CHANGING gv_text.

      " Set Text Label
      IF gv_text IS NOT INITIAL.
        ASSIGN (screen-name) TO <gfs_variable>.

        IF <gfs_variable> IS ASSIGNED.
          <gfs_variable> = gs_report_texts-text.
          UNASSIGN <gfs_variable>.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_scope.
  PERFORM f4_scope_value_request USING 'P_SCOPE' CHANGING p_scope .
*  PERFORM f4_scope_value_request USING 'P_SCOPE' CHANGING p_scope.
