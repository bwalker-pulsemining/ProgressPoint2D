DEFINE TEMP-TABLE ttPlugins no-undo
    FIELD pluginSeq         AS INTEGER
    FIELD pluginName        AS CHARACTER
    FIELD isActive          AS LOGICAL
    INDEX main IS PRIMARY UNIQUE
        pluginName.
