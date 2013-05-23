(ns jest.util.json-validation
  "JSON validation utility functions."
  (:require [clojure.data.json :as json])
  (:import com.fasterxml.jackson.databind.JsonNode
           [com.github.fge.jsonschema.main JsonSchema JsonSchemaFactory]
           com.fasterxml.jackson.databind.node.JsonNodeFactory
           com.github.fge.jackson.JsonLoader
           com.github.fge.jsonschema.report.ProcessingReport
           ))

(defn validator
  "Parses a Json-resource as a Json-schema, and returns a function that
  validates Json-resources for conformance to the schema"
  [json-resource]
  (let [fact (JsonSchemaFactory/byDefault)
        schema (.getJsonSchema fact json-resource)]
      #(.validate schema %1)))

(defn json-resource
  "Parses a Json file and returns the JsonNode"
  [path]
  (JsonLoader/fromPath path))

(defn json->JsonNode
  [json]
  2)
