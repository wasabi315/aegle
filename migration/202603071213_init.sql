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

CREATE MATERIALIZED VIEW unqual_name_resolution AS
SELECT
    e.export_as_unqual,
    array_agg(
        DISTINCT e.canonical_name
        ORDER BY e.canonical_name
    ) AS canonical_names
FROM exports e
GROUP BY e.export_as_unqual;

CREATE MATERIALIZED VIEW qual_name_resolution AS
SELECT
    e.export_as_qual,
    array_agg(
        DISTINCT e.canonical_name
        ORDER BY e.canonical_name
    ) AS canonical_names
FROM exports e
GROUP BY e.export_as_qual;
