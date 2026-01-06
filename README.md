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

## Installation

Import via [abapGit](https://abapgit.org/) to your SAP system.

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

## Features

- Works with **any** text object (custom Z* included)
- No dependency on ESH replication
- Proper ITF format conversion via `CONVERT_ITF_TO_STREAM`
- Server-side paging support
- S/4HANA compatible

## License

MIT
