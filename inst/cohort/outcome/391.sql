{
	"cdmVersionRange" : ">=5.0.0",
	"PrimaryCriteria" : {
		"CriteriaList" : [
			{
				"ConditionOccurrence" : {
					"ConditionTypeExclude" : false,
					"ConditionSourceConcept" : 1
				}
			}
		],
		"ObservationWindow" : {
			"PriorDays" : 0,
			"PostDays" : 0
		},
		"PrimaryCriteriaLimit" : {
			"Type" : "All"
		}
	},
	"ConceptSets" : [
		{
			"id" : 1,
			"name" : "Deep vein thrombosis FDA ICD10CM source concepts",
			"expression" : {
				"items" : [
					{
						"concept" : {
							"CONCEPT_ID" : 1553773,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of calf muscular vein, bilateral",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.463",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45538456,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of deep veins of left upper extremity",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.622",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45601094,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of deep veins of right upper extremity",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.621",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45548082,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of deep veins of unspecified upper extremity",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.629",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45543233,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of deep veins of upper extremity, bilateral",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.623",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45533508,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of femoral vein, bilateral",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.413",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45605849,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of iliac vein, bilateral",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.423",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45562404,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of inferior vena cava",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.220",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 1553772,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of left calf muscular vein",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.462",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45586635,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of left femoral vein",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.412",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45538454,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of left iliac vein",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.422",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 1553767,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of left peroneal vein",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.452",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45552851,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of left popliteal vein",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.432",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45552852,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of left tibial vein",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.442",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45596253,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of other specified deep vein of left lower extremity",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.492",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45567235,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of other specified deep vein of lower extremity, bilateral",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.493",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45533510,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of other specified deep vein of right lower extremity",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.491",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45538455,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of other specified deep vein of unspecified lower extremity",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.499",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 1553768,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of peroneal vein, bilateral",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.453",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45596251,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of popliteal vein, bilateral",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.433",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 1553771,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of right calf muscular vein",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.461",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45581820,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of right femoral vein",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.411",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45548080,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of right iliac vein",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.421",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 1553766,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of right peroneal vein",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.451",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45562405,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of right popliteal vein",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.431",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45596252,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of right tibial vein",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.441",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45581821,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of tibial vein, bilateral",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.443",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 1553774,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of unspecified calf muscular vein",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.469",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45605851,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of unspecified deep veins of distal lower extremity, bilateral",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.4Z3",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45572139,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of unspecified deep veins of left distal lower extremity",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.4Z2",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45538453,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of unspecified deep veins of left lower extremity",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.402",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45572138,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of unspecified deep veins of left proximal lower extremity",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.4Y2",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45576922,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of unspecified deep veins of lower extremity, bilateral",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.403",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45576923,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of unspecified deep veins of proximal lower extremity, bilateral",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.4Y3",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45557597,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of unspecified deep veins of right distal lower extremity",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.4Z1",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45543229,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of unspecified deep veins of right lower extremity",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.401",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45601089,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of unspecified deep veins of right proximal lower extremity",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.4Y1",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45586636,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of unspecified deep veins of unspecified distal lower extremity",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.4Z9",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45567234,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of unspecified deep veins of unspecified lower extremity",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.409",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45605850,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of unspecified deep veins of unspecified proximal lower extremity",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.4Y9",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45605848,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of unspecified femoral vein",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.419",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45596250,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of unspecified iliac vein",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.429",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 1553769,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of unspecified peroneal vein",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.459",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45533509,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of unspecified popliteal vein",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.439",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 45562406,
							"CONCEPT_NAME" : "Acute embolism and thrombosis of unspecified tibial vein",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.449",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "6-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 35207891,
							"CONCEPT_NAME" : "Embolism and thrombosis of renal vein",
							"STANDARD_CONCEPT" : "N",
							"STANDARD_CONCEPT_CAPTION" : "Non-Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "I82.3",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "ICD10CM",
							"CONCEPT_CLASS_ID" : "4-char billing code"
						},
						"isExcluded" : false,
						"includeDescendants" : false,
						"includeMapped" : false
					}
				]
			}
		}
	],
	"QualifiedLimit" : {
		"Type" : "All"
	},
	"ExpressionLimit" : {
		"Type" : "All"
	},
	"InclusionRules" : [
		{
			"name" : "has no events in prior 'clean window' - 365 days",
			"expression" : {
				"Type" : "ALL",
				"CriteriaList" : [
					{
						"Criteria" : {
							"ConditionOccurrence" : {
								"ConditionTypeExclude" : false,
								"ConditionSourceConcept" : 1
							}
						},
						"StartWindow" : {
							"Start" : {
								"Days" : 365,
								"Coeff" : -1
							},
							"End" : {
								"Days" : 1,
								"Coeff" : -1
							},
							"UseIndexEnd" : false,
							"UseEventEnd" : false
						},
						"RestrictVisit" : false,
						"IgnoreObservationPeriod" : true,
						"Occurrence" : {
							"Type" : 0,
							"Count" : 0,
							"IsDistinct" : false
						}
					}
				],
				"DemographicCriteriaList" : [],
				"Groups" : []
			}
		}
	],
	"EndStrategy" : {
		"DateOffset" : {
			"DateField" : "StartDate",
			"Offset" : 1
		}
	},
	"CensoringCriteria" : [],
	"CollapseSettings" : {
		"CollapseType" : "ERA",
		"EraPad" : 0
	},
	"CensorWindow" : {}
}
