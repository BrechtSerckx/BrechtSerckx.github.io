README
===

## Structure #2

- page vs template
- each can inherit from a template (cycle-checking!)
- pages are compiled to their path -> `**/*.page.html`
- page body <> page metadata <> template metadata <> parent-template metadata <> settings
- aeson value parsing is through HO function
- load aeson object as external body + metadata

## Personal website structure

- concept of pages
- default template with multiple partials:
  - main body
  - page title
  - possible modals
  - ...
- each page implements these partials, if applicable, as files
- page = a directory where a `config.yaml` is found
  - title from config
  - body as `body.html` or `body.md`
  - modals as `modals.html`
  
## Tree

```
root/
- default.html
- pages/
  - index/
    - body.html
    - config.yaml
```
