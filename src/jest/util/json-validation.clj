(ns jest.util.json-validation
  "JSON validation utility functions."
  (:require [clojure.data.json :as json])
  (:import com.fasterxml.jackson.databind.JsonNode
           [com.github.fge.jsonschema.main JsonSchema JsonSchemaFactory]
           com.github.fge.jackson.JsonLoader
           com.github.fge.jsonschema.report.ProcessingReport))

(defn- JsonNode->data
  "Converts a jackson JsonNode to clojure data"
  [node]
  (json/read-str (.toString node)
                 :key-fn keyword))

(defn- data->JsonNode
  "Converts clojure data to a jackson JsonNode"
  [data]
  (JsonLoader/fromString
   (json/write-str data)))

(defn validator
  "Parses a Json-schema, and returns a function that
  validates Json-data for conformance to the schema"
  [json-schema]
  (let [fact (JsonSchemaFactory/byDefault)
        schema (.getJsonSchema fact (data->JsonNode json-schema))]
      (fn [json-data on-success on-failure]
        (let [report (.validate schema (data->JsonNode json-data))
              success? #(.isSuccess report)
              report-json (JsonNode->data (.asJson report))]
          (apply #(partial %1 %2)
                 (if (success?)
                   [on-success json-data]
                   [on-failure report-json]))))))
