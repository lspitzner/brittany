instance HasDependencies SomeDataModel where
  -- N.B. Here is a bunch of explanatory context about the relationship
  -- between these data models or whatever.
  type Dependencies SomeDataModel
    = (SomeOtherDataModelId, SomeOtherOtherDataModelId)
