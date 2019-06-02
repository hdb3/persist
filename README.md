# Persist
Long-lived simple data repository.
Initial use - recording which of a range of addresses is already in use.
## Concept
A persistent file based data structure (other backing methods may be available).
On initialisation the structure is named, created and populated.  Operations available are essentially just ‘update’.
The idea is similar to an MVar with a longer life-cycle.
## API
* Initialisation - requires a name (String) and a data value which is a member of classes Read and Show.
* Access - read, get, put, update. Get locks the item until a put is performed.  Put before get simply replaces an old value with a new. Get and read fail after a get without a put.
* Destroy - after this operation no state from initialisation or access remains.
