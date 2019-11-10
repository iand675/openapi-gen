module JsonTest where

import Control.Monad
import Data.Aeson
import Data.Aeson.QQ
import Data.Aeson.Types
import Data.Either
import Data.Proxy
import Data.Yaml.TH
import OpenAPI.Gen.Reader
import OpenAPI.Types
import Test.Tasty.HUnit

{-
xspec_openapi_decode_then_encode_is_idempotent :: Spec
xspec_openapi_decode_then_encode_is_idempotent = it "can roundtrip idempotently" $ do
  -- A real blob
  (Right jsonBlob) <- eitherDecodeFileStrict' "openapi/openapi/spec3.json"
  -- The blob as Root
  (Right theRoot) <- eitherDecodeFileStrict' "openapi/openapi/spec3.json"
  toJSON (theRoot :: Root) `shouldBe` jsonBlob
-}

decodeWorks :: forall a. FromJSON a => Proxy a -> Value -> Assertion
decodeWorks _ v = case res of
  Left err -> assertFailure ("Decoding should have succeeded:\n" ++ err)
  Right _ -> pure ()
  where
    res = parseEither parseJSON v :: Either String a

unit_decode_empty_RefMap_Unresolved :: Assertion
unit_decode_empty_RefMap_Unresolved = decodeWorks (Proxy @(RefMap Unresolved (Schema Unresolved))) [aesonQQ|{}|]

unit_decode_small_RefMap_Unresolved :: Assertion
unit_decode_small_RefMap_Unresolved = decodeWorks (Proxy @(RefMap Unresolved (Schema Unresolved))) [aesonQQ|
{
  "dataSetList": {
    "type": "object",
    "properties": {
      "total": {
        "type": "integer"
      },
      "apis": {
        "type": "array",
        "items": {
          "type": "object",
          "properties": {
            "apiKey": {
              "type": "string",
              "description": "Woo"
            }
          }
        }
      }
    }
  }
}
|]

serverJson :: Value
serverJson = [yamlQQ|
url: '{scheme}://developer.uspto.gov/ds-api'
variables:
  scheme:
    description: 'The Data Set API is accessible via https and http'
    enum:
      - 'https'
      - 'http'
    default: 'https'
|]

unit_decode_Server :: Assertion
unit_decode_Server = decodeWorks (Proxy @Server) serverJson

infoJson :: Value
infoJson = [yamlQQ|
description: >-
  The Data Set API (DSAPI) allows the public users to discover and search
  USPTO exported data sets. This is a generic API that allows USPTO users to
  make any CSV based data files searchable through API. With the help of GET
  call, it returns the list of data fields that are searchable. With the help
  of POST call, data can be fetched based on the filters on the field names.
  Please note that POST call is used to search the actual data. The reason for
  the POST call is that it allows users to specify any complex search criteria
  without worry about the GET size limitations as well as encoding of the
  input parameters.
version: 1.0.0
title: USPTO Data Set API
contact:
  name: Open Data Portal
  url: 'https://developer.uspto.gov'
  email: developer@uspto.gov
|]

unit_decode_Info :: Assertion
unit_decode_Info = decodeWorks (Proxy @Info) infoJson

tagJson :: Value
tagJson = [yamlQQ|
name: metadata
description: Find out about the data sets
|]

pathsJson :: Value
pathsJson = [yamlQQ|
/:
  get:
    tags:
      - metadata
    operationId: list-data-sets
    summary: List available data sets
    responses:
      '200':
        description: Returns a list of data sets
        content:
          application/json:
            schema:
              $ref: '#/components/schemas/dataSetList'
            example:
              {
                "total": 2,
                "apis": [
                  {
                    "apiKey": "oa_citations",
                    "apiVersionNumber": "v1",
                    "apiUrl": "https://developer.uspto.gov/ds-api/oa_citations/v1/fields",
                    "apiDocumentationUrl": "https://developer.uspto.gov/ds-api-docs/index.html?url=https://developer.uspto.gov/ds-api/swagger/docs/oa_citations.json"
                  },
                  {
                    "apiKey": "cancer_moonshot",
                    "apiVersionNumber": "v1",
                    "apiUrl": "https://developer.uspto.gov/ds-api/cancer_moonshot/v1/fields",
                    "apiDocumentationUrl": "https://developer.uspto.gov/ds-api-docs/index.html?url=https://developer.uspto.gov/ds-api/swagger/docs/cancer_moonshot.json"
                  }
                ]
              }
/{dataset}/{version}/fields:
  get:
    tags:
      - metadata
    summary: >-
      Provides the general information about the API and the list of fields
      that can be used to query the dataset.
    description: >-
      This GET API returns the list of all the searchable field names that are
      in the oa_citations. Please see the 'fields' attribute which returns an
      array of field names. Each field or a combination of fields can be
      searched using the syntax options shown below.
    operationId: list-searchable-fields
    parameters:
      - name: dataset
        in: path
        description: 'Name of the dataset.'
        required: true
        example: "oa_citations"
        schema:
          type: string
      - name: version
        in: path
        description: Version of the dataset.
        required: true
        example: "v1"
        schema:
          type: string
    responses:
      '200':
        description: >-
          The dataset API for the given version is found and it is accessible
          to consume.
        content:
          application/json:
            schema:
              type: string
      '404':
        description: >-
          The combination of dataset name and version is not found in the
          system or it is not published yet to be consumed by public.
        content:
          application/json:
            schema:
              type: string
/{dataset}/{version}/records:
  post:
    tags:
      - search
    summary: >-
      Provides search capability for the data set with the given search
      criteria.
    description: >-
      This API is based on Solr/Lucense Search. The data is indexed using
      SOLR. This GET API returns the list of all the searchable field names
      that are in the Solr Index. Please see the 'fields' attribute which
      returns an array of field names. Each field or a combination of fields
      can be searched using the Solr/Lucene Syntax. Please refer
      https://lucene.apache.org/core/3_6_2/queryparsersyntax.html#Overview for
      the query syntax. List of field names that are searchable can be
      determined using above GET api.
    operationId: perform-search
    parameters:
      - name: version
        in: path
        description: Version of the dataset.
        required: true
        schema:
          type: string
          default: v1
      - name: dataset
        in: path
        description: 'Name of the dataset. In this case, the default value is oa_citations'
        required: true
        schema:
          type: string
          default: oa_citations
    responses:
      '200':
        description: successful operation
        content:
          application/json:
            schema:
              type: array
              items:
                type: object
                additionalProperties:
                  type: object
      '404':
        description: No matching record found for the given criteria.
    requestBody:
      content:
        application/x-www-form-urlencoded:
          schema:
            type: object
            properties:
              criteria:
                description: >-
                  Uses Lucene Query Syntax in the format of
                  propertyName:value, propertyName:[num1 TO num2] and date
                  range format: propertyName:[yyyyMMdd TO yyyyMMdd]. In the
                  response please see the 'docs' element which has the list of
                  record objects. Each record structure would consist of all
                  the fields and their corresponding values.
                type: string
                default: '*:*'
              start:
                description: Starting record number. Default value is 0.
                type: integer
                default: 0
              rows:
                description: >-
                  Specify number of rows to be returned. If you run the search
                  with default values, in the response you will see 'numFound'
                  attribute which will tell the number of records available in
                  the dataset.
                type: integer
                default: 100
            required:
              - criteria
|]

{-
  dataSetList:
    type: object
    properties:
      total:
        type: integer
      apis:
        type: array
        items:
          type: object
          properties:
            apiKey:
              type: string
              description: To be used as a dataset parameter value
            apiVersionNumber:
              type: string
              description: To be used as a version parameter value
            apiUrl:
              type: string
              format: uriref
              description: "The URL describing the dataset's fields"
            apiDocumentationUrl:
              type: string
              format: uriref
              description: A URL to the API console for each API
-}

unit_decode_optRefMap :: Assertion
unit_decode_optRefMap = case parseEither simpleParse testObject of
  Left err -> assertFailure err
  Right _ -> pure ()
  where
    simpleParse :: Value -> Parser (RefMap Unresolved Object)
    simpleParse = withObject "Test" (killer (optRefMap "noodle"))
    testObject = [aesonQQ|{"noodle": {}}|]

componentsJson :: Value
componentsJson = [aesonQQ|
{
  "schemas": {},
  "responses": {},
  "parameters": {},
  "examples": {},
  "requestBodies": {},
  "headers": {},
  "securitySchemes": {},
  "links": {},
  "callbacks": {}
}
|]


unit_decode_Components_Unresolved :: Assertion
unit_decode_Components_Unresolved = decodeWorks (Proxy @(Components Unresolved)) componentsJson

unit_decode_Components_Resolved :: Assertion
unit_decode_Components_Resolved = decodeWorks (Proxy @(Components Resolved)) componentsJson

unit_decode_Schema :: Assertion
unit_decode_Schema = decodeWorks (Proxy @(Schema Unresolved)) [yamlQQ|
type: object
properties:
  total:
    type: integer
  apis:
    type: array
    items:
      type: object
      properties:
        apiKey:
          type: string
          description: To be used as a dataset parameter value
        apiVersionNumber:
          type: string
          description: To be used as a version parameter value
        apiUrl:
          type: string
          format: uriref
          description: "The URL describing the dataset's fields"
        apiDocumentationUrl:
          type: string
          format: uriref
          description: A URL to the API console for each API
|]

unit_decode_stripe_schema :: Assertion
unit_decode_stripe_schema = void $ readSpec
