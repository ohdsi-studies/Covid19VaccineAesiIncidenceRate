{
	"cdmVersionRange" : ">=5.0.0",
	"PrimaryCriteria" : {
		"CriteriaList" : [
			{
				"Measurement" : {
					"CodesetId" : 2,
					"MeasurementTypeExclude" : false,
					"ValueAsNumber" : {
						"Value" : 10,
						"Op" : "bt",
						"Extent" : 150
					},
					"Unit" : [
						{
							"CONCEPT_ID" : 8848,
							"CONCEPT_NAME" : "thousand per microliter",
							"STANDARD_CONCEPT" : null,
							"STANDARD_CONCEPT_CAPTION" : "Unknown",
							"INVALID_REASON" : null,
							"INVALID_REASON_CAPTION" : "Unknown",
							"CONCEPT_CODE" : "10*3/uL",
							"DOMAIN_ID" : "Unit",
							"VOCABULARY_ID" : "UCUM",
							"CONCEPT_CLASS_ID" : null
						},
						{
							"CONCEPT_ID" : 8961,
							"CONCEPT_NAME" : "thousand per cubic millimeter",
							"STANDARD_CONCEPT" : null,
							"STANDARD_CONCEPT_CAPTION" : "Unknown",
							"INVALID_REASON" : null,
							"INVALID_REASON_CAPTION" : "Unknown",
							"CONCEPT_CODE" : "10*3/mm3",
							"DOMAIN_ID" : "Unit",
							"VOCABULARY_ID" : "UCUM",
							"CONCEPT_CLASS_ID" : null
						},
						{
							"CONCEPT_ID" : 8785,
							"CONCEPT_NAME" : "per cubic millimeter",
							"STANDARD_CONCEPT" : null,
							"STANDARD_CONCEPT_CAPTION" : "Unknown",
							"INVALID_REASON" : null,
							"INVALID_REASON_CAPTION" : "Unknown",
							"CONCEPT_CODE" : "/mm3",
							"DOMAIN_ID" : "Unit",
							"VOCABULARY_ID" : "UCUM",
							"CONCEPT_CLASS_ID" : null
						},
						{
							"CONCEPT_ID" : 8686,
							"CONCEPT_NAME" : "cubic millimeter",
							"STANDARD_CONCEPT" : null,
							"STANDARD_CONCEPT_CAPTION" : "Unknown",
							"INVALID_REASON" : null,
							"INVALID_REASON_CAPTION" : "Unknown",
							"CONCEPT_CODE" : "mm3",
							"DOMAIN_ID" : "Unit",
							"VOCABULARY_ID" : "UCUM",
							"CONCEPT_CLASS_ID" : null
						},
						{
							"CONCEPT_ID" : 9444,
							"CONCEPT_NAME" : "billion per liter",
							"STANDARD_CONCEPT" : null,
							"STANDARD_CONCEPT_CAPTION" : "Unknown",
							"INVALID_REASON" : null,
							"INVALID_REASON_CAPTION" : "Unknown",
							"CONCEPT_CODE" : "10*9/L",
							"DOMAIN_ID" : "Unit",
							"VOCABULARY_ID" : "UCUM",
							"CONCEPT_CLASS_ID" : null
						},
						{
							"CONCEPT_ID" : 9254,
							"CONCEPT_NAME" : "per liter",
							"STANDARD_CONCEPT" : null,
							"STANDARD_CONCEPT_CAPTION" : "Unknown",
							"INVALID_REASON" : null,
							"INVALID_REASON_CAPTION" : "Unknown",
							"CONCEPT_CODE" : "/L",
							"DOMAIN_ID" : "Unit",
							"VOCABULARY_ID" : "UCUM",
							"CONCEPT_CLASS_ID" : null
						},
						{
							"CONCEPT_ID" : 8816,
							"CONCEPT_NAME" : "million per milliliter",
							"STANDARD_CONCEPT" : null,
							"STANDARD_CONCEPT_CAPTION" : "Unknown",
							"INVALID_REASON" : null,
							"INVALID_REASON_CAPTION" : "Unknown",
							"CONCEPT_CODE" : "10*6/mL",
							"DOMAIN_ID" : "Unit",
							"VOCABULARY_ID" : "UCUM",
							"CONCEPT_CLASS_ID" : null
						}
					]
				}
			},
			{
				"Measurement" : {
					"CodesetId" : 2,
					"MeasurementTypeExclude" : false,
					"ValueAsNumber" : {
						"Value" : 10,
						"Op" : "bt",
						"Extent" : 150
					},
					"RangeLow" : {
						"Value" : 130,
						"Op" : "bt",
						"Extent" : 170
					}
				}
			},
			{
				"ConditionOccurrence" : {
					"CodesetId" : 3,
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
			"id" : 2,
			"name" : "Platelet measurement",
			"expression" : {
				"items" : [
					{
						"concept" : {
							"CONCEPT_ID" : 3007461,
							"CONCEPT_NAME" : "Platelets [#/volume] in Blood",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "26515-7",
							"DOMAIN_ID" : "Measurement",
							"VOCABULARY_ID" : "LOINC",
							"CONCEPT_CLASS_ID" : "Lab Test"
						},
						"isExcluded" : false,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 3031586,
							"CONCEPT_NAME" : "Platelets [#/volume] in Blood by Estimate",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "49497-1",
							"DOMAIN_ID" : "Measurement",
							"VOCABULARY_ID" : "LOINC",
							"CONCEPT_CLASS_ID" : "Lab Test"
						},
						"isExcluded" : false,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 3024929,
							"CONCEPT_NAME" : "Platelets [#/volume] in Blood by Automated count",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "777-3",
							"DOMAIN_ID" : "Measurement",
							"VOCABULARY_ID" : "LOINC",
							"CONCEPT_CLASS_ID" : "Lab Test"
						},
						"isExcluded" : false,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 3039827,
							"CONCEPT_NAME" : "Platelets [#/volume] in Body fluid by Automated count",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "40574-6",
							"DOMAIN_ID" : "Measurement",
							"VOCABULARY_ID" : "LOINC",
							"CONCEPT_CLASS_ID" : "Lab Test"
						},
						"isExcluded" : false,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 3024386,
							"CONCEPT_NAME" : "Platelet mean volume [Entitic volume] in Blood by Rees-Ecker",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "776-5",
							"DOMAIN_ID" : "Measurement",
							"VOCABULARY_ID" : "LOINC",
							"CONCEPT_CLASS_ID" : "Lab Test"
						},
						"isExcluded" : false,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 4267147,
							"CONCEPT_NAME" : "Platelet count",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "61928009",
							"DOMAIN_ID" : "Measurement",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Procedure"
						},
						"isExcluded" : false,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 37393863,
							"CONCEPT_NAME" : "Platelet count",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "1022651000000100",
							"DOMAIN_ID" : "Measurement",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Observable Entity"
						},
						"isExcluded" : false,
						"includeDescendants" : true,
						"includeMapped" : false
					}
				]
			}
		},
		{
			"id" : 3,
			"name" : "Thrombocytopenia (narrow)",
			"expression" : {
				"items" : [
					{
						"concept" : {
							"CONCEPT_ID" : 432870,
							"CONCEPT_NAME" : "Thrombocytopenic disorder",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "302215000",
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
							"CONCEPT_ID" : 37397537,
							"CONCEPT_NAME" : "Beta thalassemia X-linked thrombocytopenia syndrome",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "718196002",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Clinical Finding"
						},
						"isExcluded" : true,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 36713112,
							"CONCEPT_NAME" : "Pancytopenia due to antineoplastic chemotherapy",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "328301000119102",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Clinical Finding"
						},
						"isExcluded" : true,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 40321716,
							"CONCEPT_NAME" : "Secondary thrombocytopenia",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "154826009",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Clinical Finding"
						},
						"isExcluded" : true,
						"includeDescendants" : false,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 435076,
							"CONCEPT_NAME" : "Transient neonatal thrombocytopenia",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "23205009",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Clinical Finding"
						},
						"isExcluded" : true,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 4239484,
							"CONCEPT_NAME" : "Acquired pancytopenia",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "5876000",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Clinical Finding"
						},
						"isExcluded" : true,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 137829,
							"CONCEPT_NAME" : "Aplastic anemia",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "306058006",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Clinical Finding"
						},
						"isExcluded" : true,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 4133981,
							"CONCEPT_NAME" : "Benign gestational thrombocytopenia",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "128090002",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Clinical Finding"
						},
						"isExcluded" : true,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 436956,
							"CONCEPT_NAME" : "Evans syndrome",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "75331009",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Clinical Finding"
						},
						"isExcluded" : true,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 4316372,
							"CONCEPT_NAME" : "HELLP syndrome",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "95605009",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Clinical Finding"
						},
						"isExcluded" : true,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 42536958,
							"CONCEPT_NAME" : "Pancytopenia caused by medication",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "736024007",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Clinical Finding"
						},
						"isExcluded" : true,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 4247776,
							"CONCEPT_NAME" : "Posttransfusion purpura",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "73162004",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Clinical Finding"
						},
						"isExcluded" : true,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 440982,
							"CONCEPT_NAME" : "Wiskott-Aldrich syndrome",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "36070007",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Clinical Finding"
						},
						"isExcluded" : true,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 42537688,
							"CONCEPT_NAME" : "Congenital thrombocytopenia",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "737221003",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Clinical Finding"
						},
						"isExcluded" : true,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 437242,
							"CONCEPT_NAME" : "Congenital thrombocytopenic purpura",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "267535004",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Clinical Finding"
						},
						"isExcluded" : true,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 4221109,
							"CONCEPT_NAME" : "Neonatal thrombocytopenia",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "82835005",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Clinical Finding"
						},
						"isExcluded" : true,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 432881,
							"CONCEPT_NAME" : "Pancytopenia",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "127034005",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Clinical Finding"
						},
						"isExcluded" : true,
						"includeDescendants" : true,
						"includeMapped" : false
					},
					{
						"concept" : {
							"CONCEPT_ID" : 4119134,
							"CONCEPT_NAME" : "Thrombocytopenic purpura",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "302873008",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Clinical Finding"
						},
						"isExcluded" : true,
						"includeDescendants" : false,
						"includeMapped" : false
					}
				]
			}
		},
		{
			"id" : 4,
			"name" : "Thrombocytopenia (broad)",
			"expression" : {
				"items" : [
					{
						"concept" : {
							"CONCEPT_ID" : 37397537,
							"CONCEPT_NAME" : "Beta thalassemia X-linked thrombocytopenia syndrome",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "718196002",
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
							"CONCEPT_ID" : 432870,
							"CONCEPT_NAME" : "Thrombocytopenic disorder",
							"STANDARD_CONCEPT" : "S",
							"STANDARD_CONCEPT_CAPTION" : "Standard",
							"INVALID_REASON" : "V",
							"INVALID_REASON_CAPTION" : "Valid",
							"CONCEPT_CODE" : "302215000",
							"DOMAIN_ID" : "Condition",
							"VOCABULARY_ID" : "SNOMED",
							"CONCEPT_CLASS_ID" : "Clinical Finding"
						},
						"isExcluded" : false,
						"includeDescendants" : true,
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
			"name" : "has no events in prior 'clean window' - 90 days",
			"expression" : {
				"Type" : "ALL",
				"CriteriaList" : [
					{
						"Criteria" : {
							"Measurement" : {
								"CodesetId" : 2,
								"MeasurementTypeExclude" : false,
								"ValueAsNumber" : {
									"Value" : 10,
									"Op" : "bt",
									"Extent" : 150
								},
								"Unit" : [
									{
										"CONCEPT_ID" : 8686,
										"CONCEPT_NAME" : "cubic millimeter",
										"STANDARD_CONCEPT" : null,
										"STANDARD_CONCEPT_CAPTION" : "Unknown",
										"INVALID_REASON" : null,
										"INVALID_REASON_CAPTION" : "Unknown",
										"CONCEPT_CODE" : "mm3",
										"DOMAIN_ID" : "Unit",
										"VOCABULARY_ID" : "UCUM",
										"CONCEPT_CLASS_ID" : null
									},
									{
										"CONCEPT_ID" : 8785,
										"CONCEPT_NAME" : "per cubic millimeter",
										"STANDARD_CONCEPT" : null,
										"STANDARD_CONCEPT_CAPTION" : "Unknown",
										"INVALID_REASON" : null,
										"INVALID_REASON_CAPTION" : "Unknown",
										"CONCEPT_CODE" : "/mm3",
										"DOMAIN_ID" : "Unit",
										"VOCABULARY_ID" : "UCUM",
										"CONCEPT_CLASS_ID" : null
									},
									{
										"CONCEPT_ID" : 8848,
										"CONCEPT_NAME" : "thousand per microliter",
										"STANDARD_CONCEPT" : null,
										"STANDARD_CONCEPT_CAPTION" : "Unknown",
										"INVALID_REASON" : null,
										"INVALID_REASON_CAPTION" : "Unknown",
										"CONCEPT_CODE" : "10*3/uL",
										"DOMAIN_ID" : "Unit",
										"VOCABULARY_ID" : "UCUM",
										"CONCEPT_CLASS_ID" : null
									},
									{
										"CONCEPT_ID" : 8961,
										"CONCEPT_NAME" : "thousand per cubic millimeter",
										"STANDARD_CONCEPT" : null,
										"STANDARD_CONCEPT_CAPTION" : "Unknown",
										"INVALID_REASON" : null,
										"INVALID_REASON_CAPTION" : "Unknown",
										"CONCEPT_CODE" : "10*3/mm3",
										"DOMAIN_ID" : "Unit",
										"VOCABULARY_ID" : "UCUM",
										"CONCEPT_CLASS_ID" : null
									},
									{
										"CONCEPT_ID" : 9444,
										"CONCEPT_NAME" : "billion per liter",
										"STANDARD_CONCEPT" : null,
										"STANDARD_CONCEPT_CAPTION" : "Unknown",
										"INVALID_REASON" : null,
										"INVALID_REASON_CAPTION" : "Unknown",
										"CONCEPT_CODE" : "10*9/L",
										"DOMAIN_ID" : "Unit",
										"VOCABULARY_ID" : "UCUM",
										"CONCEPT_CLASS_ID" : null
									},
									{
										"CONCEPT_ID" : 9254,
										"CONCEPT_NAME" : "per liter",
										"STANDARD_CONCEPT" : null,
										"STANDARD_CONCEPT_CAPTION" : "Unknown",
										"INVALID_REASON" : null,
										"INVALID_REASON_CAPTION" : "Unknown",
										"CONCEPT_CODE" : "/L",
										"DOMAIN_ID" : "Unit",
										"VOCABULARY_ID" : "UCUM",
										"CONCEPT_CLASS_ID" : null
									},
									{
										"CONCEPT_ID" : 8816,
										"CONCEPT_NAME" : "million per milliliter",
										"STANDARD_CONCEPT" : null,
										"STANDARD_CONCEPT_CAPTION" : "Unknown",
										"INVALID_REASON" : null,
										"INVALID_REASON_CAPTION" : "Unknown",
										"CONCEPT_CODE" : "10*6/mL",
										"DOMAIN_ID" : "Unit",
										"VOCABULARY_ID" : "UCUM",
										"CONCEPT_CLASS_ID" : null
									}
								]
							}
						},
						"StartWindow" : {
							"Start" : {
								"Days" : 90,
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
						"IgnoreObservationPeriod" : false,
						"Occurrence" : {
							"Type" : 0,
							"Count" : 0,
							"IsDistinct" : false
						}
					},
					{
						"Criteria" : {
							"Measurement" : {
								"CodesetId" : 2,
								"MeasurementTypeExclude" : false,
								"ValueAsNumber" : {
									"Value" : 10,
									"Op" : "bt",
									"Extent" : 150
								},
								"RangeLow" : {
									"Value" : 130,
									"Op" : "bt",
									"Extent" : 170
								}
							}
						},
						"StartWindow" : {
							"Start" : {
								"Days" : 90,
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
						"IgnoreObservationPeriod" : false,
						"Occurrence" : {
							"Type" : 0,
							"Count" : 0,
							"IsDistinct" : false
						}
					},
					{
						"Criteria" : {
							"ConditionOccurrence" : {
								"CodesetId" : 4,
								"ConditionTypeExclude" : false
							}
						},
						"StartWindow" : {
							"Start" : {
								"Days" : 90,
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
						"IgnoreObservationPeriod" : false,
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
