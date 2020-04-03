*&---------------------------------------------------------------------*
*& Report  Z_SELECT_TVARV                                                     *
*&---------------------------------------------------------------------*
*& Example Select Single return value in parameter STVARV              *
*&---------------------------------------------------------------------*

REPORT Z_TVARV.

tables: tvarvc.

constants: c_tvarvc_person_load type rvari_vnam value 'AUDI',
           c_tvarvc_person_load_type type rsscr_kind value 'P',
           c_person_load_default type tvarv_val value 'X'.

data: l_person_load type tvarv_val.

* Verifica se um valor padrão está atribuído.
l_person_load = c_person_load_default.

* Retorna o valor do parâmetro na TVARVC.
select single low
       from tvarvc
       into l_person_load
       where name = c_tvarvc_person_load
         and type = c_tvarvc_person_load_type.

* Resultado esperado:
* L_PERSON_LOAD = DUMMY
  BREAK-POINT.