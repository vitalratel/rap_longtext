# RAP Long Text

ABAP RAP solution for reading long texts from STXH/STXL tables - works with **any** text object including custom Z* objects.

## The Problem

SAP doesn't provide a standard RAP-compatible API for custom long texts. The `I_TextObjectPlainLongText` view only works with SAP-registered text objects replicated via `ESH_SR_LTXT_REPLICATE` - custom Z objects aren't supported.

## Solution

Two approaches included:

| Object | Approach | Use Case |
|--------|----------|----------|
| `ZI_RAP_LONGTEXT` + `ZCL_RAP_LONGTEXT_QUERY` | Custom Entity with `IF_RAP_QUERY_PROVIDER` | Standalone long text queries |
| `ZCL_RAP_LONGTEXT_EXIT` | Virtual Element with `IF_SADL_EXIT_CALC_ELEMENT_READ` | Add long text field to existing CDS views |

## Architecture

```
┌─────────────────────────┐   ┌─────────────────────────┐
│ ZCL_RAP_LONGTEXT_EXIT   │   │ ZCL_RAP_LONGTEXT_QUERY  │
│ (SADL thin wrapper)     │   │ (RAP thin wrapper)      │
└───────────┬─────────────┘   └───────────┬─────────────┘
            │                             │
            └──────────┬──────────────────┘
                       ▼
         ┌─────────────────────────────┐
         │   ZIF_LONGTEXT_SERVICE      │ ← Interface
         │   + read_texts()            │
         │   + read_texts_with_paging()│
         └──────────────┬──────────────┘
                        │
                        ▼
         ┌─────────────────────────────┐
         │   ZCL_LONGTEXT_SERVICE      │ ← Implementation
         │   - READ_MULTIPLE_TEXTS     │
         │   - CONVERT_ITF_TO_STREAM   │
         └─────────────────────────────┘
```

**Benefits:**
- **Testable** - Mock the interface for unit tests
- **Reusable** - Use the service class from any context
- **Single Responsibility** - Framework code separated from business logic

## Objects

| Object | Type | Description |
|--------|------|-------------|
| `ZIF_LONGTEXT_SERVICE` | Interface | Service contract for text reading |
| `ZCL_LONGTEXT_SERVICE` | Class | Production implementation |
| `ZCL_RAP_LONGTEXT_EXIT` | Class | SADL exit for virtual elements |
| `ZCL_RAP_LONGTEXT_QUERY` | Class | RAP query provider |
| `ZI_RAP_LONGTEXT` | CDS View | Custom entity definition |

## Installation

Import via [abapGit](https://abapgit.org/) to package `$RAP_LONGTEXT`.

## Usage

### Custom Entity (standalone queries)

Query long texts directly:

```
GET /sap/opu/odata4/sap/.../ZI_RAP_LONGTEXT?$filter=TextObject eq 'ZORDER' and TextId eq 'Z001'
```

### Virtual Element (add to existing view)

Add to your CDS view:

```cds
@ObjectModel.virtualElement: true
@ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_RAP_LONGTEXT_EXIT'
LongText : abap.string;
```

Requires key fields in your view: `TextObject`, `TextName`, `TextId`, `Language`.

### Direct Service Usage

Use the service class directly in your ABAP code:

```abap
DATA(lo_service) = NEW zcl_longtext_service( ).

" Read by keys
DATA(lt_keys) = VALUE zif_longtext_service=>ty_text_keys(
  ( textobject = 'ZORDER' textname = '0000001234' textid = 'Z001' language = 'E' ) ).

DATA(lt_results) = lo_service->read_texts( lt_keys ).

" Read with paging
DATA(ls_result) = lo_service->read_texts_with_paging(
  ir_textobject = VALUE #( ( sign = 'I' option = 'EQ' low = 'ZORDER' ) )
  is_paging     = VALUE #( offset = 0 limit = 100 ) ).
```

## Features

- Works with **any** text object (custom Z* included)
- No dependency on ESH replication
- Proper ITF format conversion via `CONVERT_ITF_TO_STREAM`
- Server-side paging support (max 1000 rows)
- Optimized bulk reading via `READ_MULTIPLE_TEXTS`
- Deduplication via hashed tables for efficient range building
- O(log n) lookups via sorted tables
- Skips text retrieval when `LONGTEXT` field not requested
- S/4HANA compatible

## Platform Compatibility

| Platform | Support | Notes |
|----------|---------|-------|
| S/4HANA On-Premise | ✅ Full | All features available |
| S/4HANA Private Cloud | ✅ Full | All features available |
| S/4HANA Public Cloud | ❌ None | Classic APIs not released |
| BTP ABAP Environment | ❌ None | Classic APIs not released |

### ATC Warnings

ATC checks will report "Usage of not released ABAP Platform APIs" for:
- Types: `tdobject`, `tdobname`, `tdid`, `spras`, `tline_tab`, `text_lh`, `thead`
- Function modules: `READ_MULTIPLE_TEXTS`, `CONVERT_ITF_TO_STREAM_TEXT`
- Tables: `STXH`

These warnings are **expected and acceptable** for on-premise/private cloud deployments. The classic SAPScript text APIs are not released for ABAP Cloud, which is why this solution exists - the released `I_TextObjectPlainLongText` view only works with SAP-registered text objects.

## Testing

The service class includes unit tests. Inject a mock implementation via constructor:

```abap
" Production
DATA(lo_exit) = NEW zcl_rap_longtext_exit( ).

" Testing with mock
DATA(lo_mock) = NEW lcl_mock_service( ).
DATA(lo_exit) = NEW zcl_rap_longtext_exit( io_service = lo_mock ).
```

## License

MIT
