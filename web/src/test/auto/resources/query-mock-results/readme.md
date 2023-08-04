Note that the `timerid` field is omitted from these objects.

It's difficult to mock within these files due to aspects of the current design - the requester-objects
autogenerate a value dynamically, and in production, said value is reflected in that field.