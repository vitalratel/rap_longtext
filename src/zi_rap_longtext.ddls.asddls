@EndUserText.label: 'Long Text Custom Entity'
@ObjectModel.query.implementedBy: 'ABAP:ZCL_RAP_LONGTEXT_QUERY'
define custom entity ZI_RAP_LONGTEXT
{
  key TextObject  : tdobject;
  key TextName    : tdobname;
  key TextId      : tdid;
  key Language    : spras;
      Title       : tdtitle;
      CreatedBy   : tdfuser;
      CreatedDate : tdfdate;
      ChangedBy   : tdluser;
      ChangedDate : tdldate;
      LongText    : abap.string;
}
