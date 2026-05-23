CREATE TYPE return_type_head AS ENUM (
    'U',
    'Var',
    'Top',
    'Sigma',
    'Proj1',
    'Proj2',
    'Unknown'
);

CREATE TYPE polymorphic AS ENUM (
    'Monomorphic',
    'Polymorphic'
);

CREATE TABLE library_items (
    id                   bigint            GENERATED ALWAYS AS IDENTITY PRIMARY KEY,

    canonical_name       text              NOT NULL,  -- Can't be UNIQUE because we have overloaded record constructors
    signature            bytea             NOT NULL,
    body                 bytea,                       -- NULL for opaque definitions

    -- Features
    arity                int               NOT NULL,
    arity_has_var        boolean           NOT NULL,
    polymorphic          polymorphic       NOT NULL,
    return_type_head     return_type_head  NOT NULL,
    return_type_head_top text                        -- NULL unless return_type_head = 'Top'

    CONSTRAINT return_type_head_top_check CHECK (
        (return_type_head = 'Top'  AND return_type_head_top IS NOT NULL) OR
        (return_type_head <> 'Top' AND return_type_head_top IS NULL)
    )
);

CREATE TABLE exports (
    id                   bigint            GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    canonical_name       text              NOT NULL,
    export_as_qual       text              NOT NULL,
    export_as_unqual     text              NOT NULL
);

CREATE MATERIALIZED VIEW exports_unqual AS
SELECT DISTINCT export_as_unqual, canonical_name
FROM exports;

CREATE MATERIALIZED VIEW exports_qual AS
SELECT DISTINCT export_as_qual, canonical_name
FROM exports;
