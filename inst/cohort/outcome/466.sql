{
	"cdmVersionRange" : ">=5.0.0",
	"PrimaryCriteria" : {
		"CriteriaList" : [
			{
				"ConditionOccurrence" : {
					"CodesetId" : 1,
					"ConditionTypeExclude" : false
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
			"name" : "[Vaccine AESI] Intraabdominal thrombosis - hepatic",
			"expression" : {
				"items" : [
					{
						"concept" : {
							"CONCEPT_ID" : 4223098,
							"CONCEPT_NAME" : "Hepatic artery thrombosis",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "83940008",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Clinical Finding"
						},
						"isExcluded" : false,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 4301208,
							"CONCEPT_NAME" : "Hepatic vein thrombosis",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "38739001",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Clinical Finding"
						},
						"isExcluded" : false,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 4111853,
							"CONCEPT_NAME" : "Embolism and thrombosis of hepatic artery",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "195343000",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Clinical Finding"
						},
						"isExcluded" : false,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 196715,
							"CONCEPT_NAME" : "Budd-Chiari syndrome",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "82385007",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Clinical Finding"
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
								"CodesetId" : 1,
								"ConditionTypeExclude" : false
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
