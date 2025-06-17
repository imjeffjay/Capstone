# Load required libraries
library(bigrquery)
library(DBI)
library(here)
library(glue)

sql_query_admin_samp <- glue("

WITH all_admissions AS (
    -- Full admissions table for readmission calculation
    SELECT *
    FROM `physionet-data.mimiciv_3_1_hosp.admissions`
),

sampled_admissions AS (
    -- Randomly sample 50% of patients but keep all their admissions
    SELECT a.*
    FROM all_admissions AS a
    WHERE subject_id IN (
        -- Select 10% of patients but retain all their admissions
        SELECT DISTINCT subject_id
        FROM all_admissions
        WHERE MOD(ABS(FARM_FINGERPRINT(CAST(subject_id AS STRING))), 100) < {sample_percent}
    )
),

## Complications:

# WITH surgery_patients AS (
#     SELECT 
#         proc.subject_id, proc.hadm_id, 
#         MIN(proc.charttime) AS first_surgery_time,
#         1 AS surgery_flag  
#     FROM `physionet-data.mimiciv_3_1_hosp.procedures_icd` AS proc
#     WHERE proc.icd_code LIKE '0J%'  -- Example: Identify major surgical ICD-10 codes
#     GROUP BY proc.subject_id, proc.hadm_id
# )


diag AS (
    -- Extract diagnosis codes with ICD version for sampled admissions
    SELECT
        d.hadm_id,
        CASE WHEN d.icd_version = 9 THEN d.icd_code ELSE NULL END AS icd9_code,
        CASE WHEN d.icd_version = 10 THEN d.icd_code ELSE NULL END AS icd10_code
    FROM `physionet-data.mimiciv_3_1_hosp.diagnoses_icd` AS d
    WHERE d.hadm_id IN (SELECT hadm_id FROM sampled_admissions)
),

diagnoses_features AS (
    SELECT
        d.hadm_id

        -- Myocardial infarction
        , MAX(CASE WHEN
            SUBSTR(d.icd9_code, 1, 3) IN ('410', '412')
            OR
            SUBSTR(d.icd10_code, 1, 3) IN ('I21', 'I22')
            OR
            SUBSTR(d.icd10_code, 1, 4) = 'I252'
            THEN 1
            ELSE 0 END) AS myocardial_infarct

        -- Congestive heart failure
        , MAX(CASE WHEN
            SUBSTR(d.icd9_code, 1, 3) = '428'
            OR
            SUBSTR(
                icd9_code, 1, 5
            ) IN ('39891', '40201', '40211', '40291', '40401', '40403'
                , '40411', '40413', '40491', '40493')
            OR
            SUBSTR(d.icd9_code, 1, 4) BETWEEN '4254' AND '4259'
            OR
            SUBSTR(d.icd10_code, 1, 3) IN ('I43', 'I50')
            OR
            SUBSTR(
                icd10_code, 1, 4
            ) IN ('I099', 'I110', 'I130', 'I132', 'I255', 'I420'
                  , 'I425', 'I426', 'I427', 'I428', 'I429', 'P290'
            )
            THEN 1
            ELSE 0 END) AS congestive_heart_failure

        -- Peripheral vascular disease
        , MAX(CASE WHEN
            SUBSTR(d.icd9_code, 1, 3) IN ('440', '441')
            OR
            SUBSTR(
                icd9_code, 1, 4
            ) IN ('0930', '4373', '4471', '5571', '5579', 'V434')
            OR
            SUBSTR(d.icd9_code, 1, 4) BETWEEN '4431' AND '4439'
            OR
            SUBSTR(d.icd10_code, 1, 3) IN ('I70', 'I71')
            OR
            SUBSTR(d.icd10_code, 1, 4) IN ('I731', 'I738', 'I739', 'I771', 'I790'
                                         , 'I792'
                                         , 'K551'
                                         , 'K558'
                                         , 'K559'
                                         , 'Z958'
                                         , 'Z959'
            )
            THEN 1
            ELSE 0 END) AS peripheral_vascular_disease

        -- Cerebrovascular disease
        , MAX(CASE WHEN
            SUBSTR(d.icd9_code, 1, 3) BETWEEN '430' AND '438'
            OR
            SUBSTR(d.icd9_code, 1, 5) = '36234'
            OR
            SUBSTR(d.icd10_code, 1, 3) IN ('G45', 'G46')
            OR
            SUBSTR(d.icd10_code, 1, 3) BETWEEN 'I60' AND 'I69'
            OR
            SUBSTR(d.icd10_code, 1, 4) = 'H340'
            THEN 1
            ELSE 0 END) AS cerebrovascular_disease

        -- Dementia
        , MAX(CASE WHEN
            SUBSTR(d.icd9_code, 1, 3) = '290'
            OR
            SUBSTR(d.icd9_code, 1, 4) IN ('2941', '3312')
            OR
            SUBSTR(d.icd10_code, 1, 3) IN ('F00', 'F01', 'F02', 'F03', 'G30')
            OR
            SUBSTR(d.icd10_code, 1, 4) IN ('F051', 'G311')
            THEN 1
            ELSE 0 END) AS dementia

        -- Chronic pulmonary disease
        , MAX(CASE WHEN
            SUBSTR(d.icd9_code, 1, 3) BETWEEN '490' AND '505'
            OR
            SUBSTR(d.icd9_code, 1, 4) IN ('4168', '4169', '5064', '5081', '5088')
            OR
            SUBSTR(d.icd10_code, 1, 3) BETWEEN 'J40' AND 'J47'
            OR
            SUBSTR(d.icd10_code, 1, 3) BETWEEN 'J60' AND 'J67'
            OR
            SUBSTR(d.icd10_code, 1, 4) IN ('I278', 'I279', 'J684', 'J701', 'J703')
            THEN 1
            ELSE 0 END) AS chronic_pulmonary_disease

        -- Rheumatic disease
        , MAX(CASE WHEN
            SUBSTR(d.icd9_code, 1, 3) = '725'
            OR
            SUBSTR(d.icd9_code, 1, 4) IN ('4465', '7100', '7101', '7102', '7103'
                                        , '7104', '7140', '7141', '7142', '7148'
            )
            OR
            SUBSTR(d.icd10_code, 1, 3) IN ('M05', 'M06', 'M32', 'M33', 'M34')
            OR
            SUBSTR(d.icd10_code, 1, 4) IN ('M315', 'M351', 'M353', 'M360')
            THEN 1
            ELSE 0 END) AS rheumatic_disease

        -- Peptic ulcer disease
        , MAX(CASE WHEN
            SUBSTR(d.icd9_code, 1, 3) IN ('531', '532', '533', '534')
            OR
            SUBSTR(d.icd10_code, 1, 3) IN ('K25', 'K26', 'K27', 'K28')
            THEN 1
            ELSE 0 END) AS peptic_ulcer_disease

        -- Mild liver disease
        , MAX(CASE WHEN
            SUBSTR(d.icd9_code, 1, 3) IN ('570', '571')
            OR
            SUBSTR(
                icd9_code, 1, 4
            ) IN ('0706', '0709', '5733', '5734', '5738', '5739', 'V427')
            OR
            SUBSTR(
                icd9_code, 1, 5
            ) IN ('07022', '07023', '07032', '07033', '07044', '07054')
            OR
            SUBSTR(d.icd10_code, 1, 3) IN ('B18', 'K73', 'K74')
            OR
            SUBSTR(
                icd10_code, 1, 4
            ) IN ('K700', 'K701', 'K702', 'K703', 'K709', 'K713'
                  , 'K714', 'K715', 'K717', 'K760', 'K762'
                  , 'K763', 'K764', 'K768', 'K769', 'Z944')
            THEN 1
            ELSE 0 END) AS mild_liver_disease

        -- Diabetes without chronic complication
        , MAX(CASE WHEN
            SUBSTR(
                icd9_code, 1, 4
            ) IN ('2500', '2501', '2502', '2503', '2508', '2509')
            OR
            SUBSTR(
                icd10_code, 1, 4
            ) IN ('E100', 'E101', 'E106', 'E108', 'E109', 'E110', 'E111'
                  , 'E116'
                  , 'E118'
                  , 'E119'
                  , 'E120'
                  , 'E121'
                  , 'E126'
                  , 'E128'
                  , 'E129'
                  , 'E130'
                  , 'E131'
                  , 'E136'
                  , 'E138'
                  , 'E139'
                  , 'E140'
                  , 'E141', 'E146', 'E148', 'E149')
            THEN 1
            ELSE 0 END) AS diabetes_without_cc

        -- Diabetes with chronic complication
        , MAX(CASE WHEN
            SUBSTR(d.icd9_code, 1, 4) IN ('2504', '2505', '2506', '2507')
            OR
            SUBSTR(
                icd10_code, 1, 4
            ) IN ('E102', 'E103', 'E104', 'E105', 'E107', 'E112', 'E113'
                  , 'E114'
                  , 'E115'
                  , 'E117'
                  , 'E122'
                  , 'E123'
                  , 'E124'
                  , 'E125'
                  , 'E127'
                  , 'E132'
                  , 'E133'
                  , 'E134'
                  , 'E135'
                  , 'E137'
                  , 'E142'
                  , 'E143', 'E144', 'E145', 'E147')
            THEN 1
            ELSE 0 END) AS diabetes_with_cc

        -- Hemiplegia or paraplegia
        , MAX(CASE WHEN
            SUBSTR(d.icd9_code, 1, 3) IN ('342', '343')
            OR
            SUBSTR(d.icd9_code, 1, 4) IN ('3341', '3440', '3441', '3442'
                                        , '3443', '3444', '3445', '3446', '3449'
            )
            OR
            SUBSTR(d.icd10_code, 1, 3) IN ('G81', 'G82')
            OR
            SUBSTR(d.icd10_code, 1, 4) IN ('G041', 'G114', 'G801', 'G802', 'G830'
                                         , 'G831'
                                         , 'G832'
                                         , 'G833'
                                         , 'G834'
                                         , 'G839'
            )
            THEN 1
            ELSE 0 END) AS paraplegia

        -- Renal disease
        , MAX(CASE WHEN
            SUBSTR(d.icd9_code, 1, 3) IN ('582', '585', '586', 'V56')
            OR
            SUBSTR(d.icd9_code, 1, 4) IN ('5880', 'V420', 'V451')
            OR
            SUBSTR(d.icd9_code, 1, 4) BETWEEN '5830' AND '5837'
            OR
            SUBSTR(
                icd9_code, 1, 5
            ) IN (
                '40301'
                , '40311'
                , '40391'
                , '40402'
                , '40403'
                , '40412'
                , '40413'
                , '40492'
                , '40493'
            )
            OR
            SUBSTR(d.icd10_code, 1, 3) IN ('N18', 'N19')
            OR
            SUBSTR(d.icd10_code, 1, 4) IN ('I120', 'I131', 'N032', 'N033', 'N034'
                                         , 'N035'
                                         , 'N036'
                                         , 'N037'
                                         , 'N052'
                                         , 'N053'
                                         , 'N054'
                                         , 'N055'
                                         , 'N056'
                                         , 'N057'
                                         , 'N250'
                                         , 'Z490'
                                         , 'Z491'
                                         , 'Z492'
                                         , 'Z940'
                                         , 'Z992'
            )
            THEN 1 ELSE 0 END) AS renal_disease

        -- Any malignancy, including lymphoma and leukemia,
        -- except malignant neoplasm of skin.
        , MAX(CASE WHEN
            SUBSTR(d.icd9_code, 1, 3) BETWEEN '140' AND '172'
            OR
            SUBSTR(d.icd9_code, 1, 4) BETWEEN '1740' AND '1958'
            OR
            SUBSTR(d.icd9_code, 1, 3) BETWEEN '200' AND '208'
            OR
            SUBSTR(d.icd9_code, 1, 4) = '2386'
            OR
            SUBSTR(d.icd10_code, 1, 3) IN ('C43', 'C88')
            OR
            SUBSTR(d.icd10_code, 1, 3) BETWEEN 'C00' AND 'C26'
            OR
            SUBSTR(d.icd10_code, 1, 3) BETWEEN 'C30' AND 'C34'
            OR
            SUBSTR(d.icd10_code, 1, 3) BETWEEN 'C37' AND 'C41'
            OR
            SUBSTR(d.icd10_code, 1, 3) BETWEEN 'C45' AND 'C58'
            OR
            SUBSTR(d.icd10_code, 1, 3) BETWEEN 'C60' AND 'C76'
            OR
            SUBSTR(d.icd10_code, 1, 3) BETWEEN 'C81' AND 'C85'
            OR
            SUBSTR(d.icd10_code, 1, 3) BETWEEN 'C90' AND 'C97'
            THEN 1
            ELSE 0 END) AS malignant_cancer

        -- Moderate or severe liver disease
        , MAX(CASE WHEN
            SUBSTR(d.icd9_code, 1, 4) IN ('4560', '4561', '4562')
            OR
            SUBSTR(d.icd9_code, 1, 4) BETWEEN '5722' AND '5728'
            OR
            SUBSTR(
                icd10_code, 1, 4
            ) IN ('I850', 'I859', 'I864', 'I982', 'K704', 'K711'
                  , 'K721', 'K729', 'K765', 'K766', 'K767')
            THEN 1
            ELSE 0 END) AS severe_liver_disease

        -- Metastatic solid tumor
        , MAX(CASE WHEN
            SUBSTR(d.icd9_code, 1, 3) IN ('196', '197', '198', '199')
            OR
            SUBSTR(d.icd10_code, 1, 3) IN ('C77', 'C78', 'C79', 'C80')
            THEN 1
            ELSE 0 END) AS metastatic_solid_tumor

        -- AIDS/HIV
        , MAX(CASE WHEN
            SUBSTR(d.icd9_code, 1, 3) IN ('042', '043', '044')
            OR
            SUBSTR(d.icd10_code, 1, 3) IN ('B20', 'B21', 'B22', 'B24')
            THEN 1
            ELSE 0 END) AS aids
        
        -- Binary complication flag (1 if any matched diagnosis)
        , MAX(CASE 
              WHEN SUBSTR(d.icd9_code, 1, 3) IN ('410', '428', '440', '441', '430', '490', '290', '531', '250') 
              OR SUBSTR(d.icd10_code, 1, 3) IN ('I21', 'I50', 'I70', 'I71', 'G45', 'J18', 'F00', 'K25', 'E10') 
              THEN 1 ELSE 0 END) AS complication_binary

        -- Complication Severity Index (Weighted Score)
        , SUM(CASE 
              WHEN 
                SUBSTR(d.icd9_code, 1, 3) IN ('410', '428') 
                OR SUBSTR(d.icd10_code, 1, 3) IN ('I21', 'I50') THEN 5  -- Severe: Heart Attack, CHF
              WHEN 
                SUBSTR(d.icd9_code, 1, 3) IN ('440', '441', '430', '490') 
                OR SUBSTR(d.icd10_code, 1, 3) IN ('I70', 'I71', 'G45', 'J18') THEN 3  -- Moderate: Stroke, Pneumonia
              WHEN 
                SUBSTR(d.icd9_code, 1, 3) IN ('290', '531', '250') 
                OR SUBSTR(d.icd10_code, 1, 3) IN ('F00', 'K25', 'E10') THEN 2  -- Mild: Dementia, Ulcers, Diabetes
            ELSE 0 END) AS complication_index
        
        -- CMS-Tracked Conditions (AMI, HF, Pneumonia, COPD, Stroke, Sepsis)
        , MAX(CASE WHEN 
              -- Acute Myocardial Infarction (AMI)
              SUBSTR(d.icd9_code, 1, 3) IN ('410') OR SUBSTR(d.icd10_code, 1, 3) IN ('I21', 'I22')
              
              -- Heart Failure (HF)
              OR SUBSTR(d.icd9_code, 1, 3) IN ('428') OR SUBSTR(d.icd10_code, 1, 3) IN ('I50')
              
              -- Pneumonia (PN)
              OR SUBSTR(d.icd9_code, 1, 3) IN ('486') OR SUBSTR(d.icd10_code, 1, 3) IN ('J18')
              
              -- Chronic Obstructive Pulmonary Disease (COPD)
              OR SUBSTR(d.icd9_code, 1, 3) IN ('491', '492', '496') OR SUBSTR(d.icd10_code, 1, 3) IN ('J44')
              
              -- Stroke
              OR SUBSTR(d.icd9_code, 1, 3) IN ('434', '436') OR SUBSTR(d.icd10_code, 1, 3) IN ('I63', 'I64')
              
              -- Sepsis (NEW in 2024 CMS Measure)
              OR SUBSTR(d.icd9_code, 1, 3) IN ('995', '785') OR SUBSTR(d.icd10_code, 1, 3) IN ('A41', 'R65')
        THEN 1 ELSE 0 END) AS cms_flag  
      FROM diag AS d
    GROUP BY d.hadm_id
),        


admissions_features AS (
    SELECT
        a.subject_id,
        a.hadm_id,
        a.admittime,
        a.dischtime,
        a.race,
        a.admission_location,

        -- Calculate LOS
        TIMESTAMP_DIFF(a.dischtime, a.admittime, DAY) AS length_of_stay, -- Calculate LOS

        -- Capture next admission time for readmission calculation
            -- for 30-day readmission because a patient can be admitted multiple times partitioning ensures
            -- that each readmission is checked against the prior discharge event to determine if it happened within 30 days
            
        LEAD(a.admittime) OVER (PARTITION BY a.subject_id ORDER BY a.admittime ASC) AS next_admittime,
        
        -- Define 30-day readmission based on next_admittime  
        CASE
            WHEN TIMESTAMP_DIFF(
                LEAD(a.admittime) OVER (PARTITION BY a.subject_id ORDER BY a.admittime ASC),
                a.dischtime,
                DAY
            ) <= 30 THEN 1
            ELSE 0
        END AS readmitted,

            --## Readmission:
            -- https://www.cms.gov/medicare/quality/value-based-programs/hospital-readmissions
            -- https://www.cms.gov/Medicare/Medicare-Fee-for-Service-Payment/PhysicianFeedbackProgram/Downloads/2016-ACR-MIF.pdf
            -- https://www.sqlservercentral.com/forums/topic/readmission-within-30-days-from-a-discharge
    
        -- Race Binning (Moved from patient_features to admission_features)
        CASE 
            WHEN LOWER(a.race) LIKE 'white%' THEN 'White'
            WHEN LOWER(a.race) LIKE 'black%' THEN 'Black'
            WHEN LOWER(a.race) LIKE 'asian%' THEN 'Asian'
            WHEN LOWER(a.race) LIKE '%hispanic%' OR LOWER(a.race) LIKE '%latino%' THEN 'Hispanic/Latino'
            WHEN LOWER(a.race) LIKE 'american indian%' THEN 'Native American'
            WHEN LOWER(a.race) LIKE 'native hawaiian%' THEN 'Pacific Islander'
            WHEN LOWER(a.race) IN ('unknown', 'patient declined to answer', 'unable to obtain') THEN 'Unknown/Declined'
            ELSE 'Other'
        END AS race_group,

        -- Binary column for race
        CASE 
            WHEN LOWER(a.race) LIKE '%white%' THEN 1 
            ELSE 0 
        END AS is_white,
        
    FROM all_admissions AS a
),            

prior_visits AS (
    -- Compute prior visits for each subject in the last 1 year
    SELECT
        a.subject_id,
        a.hadm_id,
        COUNT(CASE 
            WHEN TIMESTAMP_DIFF(a.admittime, prev.admittime, DAY) <= 365 
            THEN 1 ELSE NULL 
        END) AS prior_hospital_visits_1yr,
        COUNT(CASE 
            WHEN TIMESTAMP_DIFF(a.admittime, prev.admittime, DAY) <= 365 
            AND prev.admission_location = 'EMERGENCY ROOM'
            THEN 1 ELSE NULL 
        END) AS prior_er_visits_1yr
    FROM all_admissions AS a
    LEFT JOIN all_admissions AS prev
        ON a.subject_id = prev.subject_id 
        AND prev.admittime < a.admittime  -- Only count **prior** admissions
    GROUP BY a.subject_id, a.hadm_id
),
  
patient_features AS (
    SELECT
        p.subject_id,
        p.dod,
        p.anchor_age,
        p.anchor_year,
        CASE 
            WHEN p.gender = 'M' THEN 'Male'
            WHEN p.gender = 'F' THEN 'Female'
            ELSE 'Unknown' -- In case of unexpected values
        END AS gender
    FROM `physionet-data.mimiciv_3_1_hosp.patients` AS p
),


icu_features AS (
    -- ICU Admission Features Aggregated per Hospital Admission
    SELECT
        i.subject_id,
        i.hadm_id,

        -- First ICU admission time within this hospital stay
        MIN(i.intime) AS first_icu_admit,

        -- Last ICU discharge time within this hospital stay
        MAX(i.outtime) AS last_icu_discharge,

        -- Total number of ICU stays per hospital admission
        COUNT(*) AS total_icu_stays,

        -- Total hours spent in ICU for this admission
        SUM(TIMESTAMP_DIFF(i.outtime, i.intime, HOUR)) AS total_icu_hours,
        
        -- ICU flag: 1 if the hospital admission includes ICU stay, 0 otherwise
        CASE WHEN COUNT(*) > 0 THEN 1 ELSE 0 END AS ICU_flag

    FROM `physionet-data.mimiciv_3_1_icu.icustays` AS i
    GROUP BY i.subject_id, i.hadm_id  
),

icu_filtered AS (
    -- Keep only first ICU stay per hospital admission
    SELECT *
    FROM `msds-795.mimiciv_derived.icustay_detail`
    WHERE first_icu_stay = TRUE
),


sapsii_max AS (
    SELECT hadm_id, MAX(sapsii) AS max_sapsii
    FROM `msds-795.mimiciv_derived.sapsii`
    GROUP BY hadm_id
),

sirs_max AS (
    SELECT hadm_id, MAX(sirs) AS max_sirs
    FROM `msds-795.mimiciv_derived.sirs`
    GROUP BY hadm_id
),

--sofa_max AS (
--    SELECT hadm_id, MAX(sofa_24hours) AS max_sofa_24hours
--    FROM `msds-795.mimiciv_derived.sofa`
--    GROUP BY hadm_id
--),

oasis_max AS (
    SELECT hadm_id, MAX(oasis) AS max_oasis
    FROM `msds-795.mimiciv_derived.oasis`
    GROUP BY hadm_id
),

lods_max AS (
    SELECT hadm_id, MAX(lods) AS max_lods
    FROM `msds-795.mimiciv_derived.lods`
    GROUP BY hadm_id
),

apsiii_max AS (
    SELECT hadm_id, MAX(apsiii) AS max_apsiii
    FROM `msds-795.mimiciv_derived.apsiii`
    GROUP BY hadm_id
),

procedures_features AS (
    SELECT
        proc.subject_id,
        proc.hadm_id,

        ----- Predicitve Procedures
    
        -- Count total procedures per hospital admission
        COUNT(*) AS num_procedures,

        -- Major Surgery Flag
        MAX(CASE WHEN proc.icd_code LIKE '0J%' THEN 1 ELSE 0 END) AS major_surgery_flag,

        -- Cardiac Surgery Flag
        MAX(CASE WHEN proc.icd_code IN ('0210093', '027034Z', '02RF07Z') THEN 1 ELSE 0 END) AS cardiac_surgery_flag,

        -- Orthopedic Surgery (Joint Replacements)
        MAX(CASE WHEN proc.icd_code LIKE '0SR%' THEN 1 ELSE 0 END) AS orthopedic_surgery_flag,

        -- Neurosurgery (Brain/Spinal Surgery)
        MAX(CASE WHEN proc.icd_code IN ('0016070', '00H00MZ') THEN 1 ELSE 0 END) AS neurosurgery_flag,

        -- Sepsis/Infection Procedure Flag
        MAX(CASE WHEN proc.icd_code IN ('6A550Z3', '5A1955Z') THEN 1 ELSE 0 END) AS sepsis_infection_flag,

        -- Dialysis Flag (Renal Failure Indicator)
        MAX(CASE WHEN proc.icd_code IN ('5A1D00Z', '5A1D60Z') THEN 1 ELSE 0 END) AS dialysis_flag,

        -- Blood Transfusion Flag (Hemorrhage Risk)
        MAX(CASE WHEN proc.icd_code IN ('30233N1', '30233P1') THEN 1 ELSE 0 END) AS blood_transfusion_flag,

        -- Emergency Surgery (High Severity)
        MAX(CASE WHEN proc.icd_code LIKE '0WQF%' THEN 1 ELSE 0 END) AS emergency_surgery_flag,

        -- Stroke Prevention (Carotid Endarterectomy)
        MAX(CASE WHEN proc.icd_code = '03CL0ZZ' THEN 1 ELSE 0 END) AS stroke_prevention_flag,

        -- Bowel Resection (Sepsis & Readmission Risk)
        MAX(CASE WHEN proc.icd_code IN ('0DBE0ZZ', '0DBF0ZZ') THEN 1 ELSE 0 END) AS bowel_resection_flag,

        -- Colostomy/Ileostomy (GI Complications)
        MAX(CASE WHEN proc.icd_code IN ('0D1B0Z4', '0D1D0Z4') THEN 1 ELSE 0 END) AS colostomy_ileostomy_flag,

        -- Gastrostomy Tube (PEG Tube, Malnutrition Risk)
        MAX(CASE WHEN proc.icd_code = '0DH63UZ' THEN 1 ELSE 0 END) AS gastrostomy_tube_flag,

        -- Central Line Placement (Sepsis Risk)
        MAX(CASE WHEN proc.icd_code IN ('02HV33Z', '02HV34Z') THEN 1 ELSE 0 END) AS central_line_flag,

        -- Wound Debridement (Infection Risk)
        MAX(CASE WHEN proc.icd_code = '0JDJ0ZZ' THEN 1 ELSE 0 END) AS wound_debridement_flag,
        
        ----- Late Stage features
    
        -- Respiratory Support (Ventilation, Intubation)
        MAX(CASE WHEN proc.icd_code IN ('5A09357', '0BH17EZ', '0BH18EZ') THEN 1 ELSE 0 END) AS respiratory_support_flag,

        -- ECMO Use (Extracorporeal Life Support)
        MAX(CASE WHEN proc.icd_code IN ('5A1522F', '5A1522G') THEN 1 ELSE 0 END) AS ecmo_flag,

        -- Prolonged Ventilation > 96 Hours
        MAX(CASE WHEN proc.icd_code = '5A0945Z' THEN 1 ELSE 0 END) AS prolonged_ventilation_flag,

        -- Tracheostomy (Long-Term Ventilation)
        MAX(CASE WHEN proc.icd_code IN ('0B110F4', '0B110Z4') THEN 1 ELSE 0 END) AS tracheostomy_flag,

        -- Shock Management (Vasopressors, Resuscitation)
        MAX(CASE WHEN proc.icd_code IN ('3E013GC', '3E013GQ') THEN 1 ELSE 0 END) AS shock_management_flag,

        -- CPR / Cardiac Arrest (Too Late to Predict)
        MAX(CASE WHEN proc.icd_code IN ('5A12012', '5A12013') THEN 1 ELSE 0 END) AS cpr_flag


    FROM `physionet-data.mimiciv_3_1_hosp.procedures_icd` AS proc
    GROUP BY proc.subject_id, proc.hadm_id
),

medications_features AS (
    SELECT
        med.subject_id,
        med.hadm_id,
        
        ----- Predicitve features:
        
        -- Count distinct medications per admission
        COUNT(DISTINCT med.drug) AS num_medications,

        -- Broad-Spectrum Antibiotics (Sepsis Risk)
        MAX(CASE WHEN med.drug IN ('Meropenem', 'Piperacillin-Tazobactam', 'Vancomycin', 'Cefepime') THEN 1 ELSE 0 END) AS broad_spectrum_antibiotic_med_flag,

        -- Antifungal Medications (Immunocompromised, High Mortality)
        MAX(CASE WHEN med.drug IN ('Amphotericin B', 'Fluconazole') THEN 1 ELSE 0 END) AS antifungal_med_flag,

        -- Inotropes (Heart Failure & Shock)
        MAX(CASE WHEN med.drug IN ('Dobutamine', 'Milrinone') THEN 1 ELSE 0 END) AS inotrope_med_flag,

        -- Antiarrhythmics (Cardiac Arrest & Stroke Risk)
        MAX(CASE WHEN med.drug = 'Amiodarone' THEN 1 ELSE 0 END) AS antiarrhythmic_med_flag,

        -- Nephrotoxic Medications (Acute Kidney Injury & Dialysis Risk)
        MAX(CASE WHEN med.drug IN ('Vancomycin', 'Gentamicin', 'Tobramycin') THEN 1 ELSE 0 END) AS nephrotoxic_med_flag,

        -- Diuretics (Heart & Kidney Failure Risk)
        MAX(CASE WHEN med.drug IN ('Furosemide', 'Bumetanide') THEN 1 ELSE 0 END) AS diuretic_med_flag,

        -- Corticosteroids (Sepsis & Inflammation)
        MAX(CASE WHEN med.drug IN ('Methylprednisolone', 'Dexamethasone', 'Hydrocortisone') THEN 1 ELSE 0 END) AS corticosteroid_med_flag,

        -- Sedative Use (Ventilation, ICU Risk)
        MAX(CASE WHEN med.drug IN ('Propofol', 'Midazolam', 'Dexmedetomidine') THEN 1 ELSE 0 END) AS sedative_med_flag,

        -- Pressor Use (Shock/ICU Support)
        MAX(CASE WHEN med.drug IN ('Norepinephrine', 'Vasopressin') THEN 1 ELSE 0 END) AS pressor_med_flag,
        
    FROM `physionet-data.mimiciv_3_1_hosp.prescriptions` AS med
    GROUP BY med.subject_id, med.hadm_id  
),

ed_features AS (
    SELECT
        es.subject_id,   
        es.hadm_id,      
        MIN(es.intime) AS ed_admit_time,
        MAX(es.outtime) AS ed_discharge_time,

        -- Count total ED visits for the patient
        COUNT(*) AS total_ed_visits,

        -- Highest Triage Acuity for This Hospital Stay
        MAX(et.acuity) AS ed_acuity,

        -- Most Frequent Chief Complaint using ARRAY_AGG
        ARRAY_AGG(et.chiefcomplaint ORDER BY et.acuity DESC LIMIT 1)[OFFSET(0)] AS ed_chief_complaint

    FROM `physionet-data.mimiciv_ed.edstays` AS es
    LEFT JOIN `physionet-data.mimiciv_ed.triage` AS et 
        ON es.stay_id = et.stay_id  
    GROUP BY es.subject_id, es.hadm_id  
)

SELECT
    sa.subject_id
    , sa.hospital_expire_flag
    , sa.hadm_id
    , sa.marital_status AS marital_status_category
    , sa.insurance AS insurance_category
    , sa.admission_location AS admission_location_category
    , sa.discharge_location AS discharge_location_category
    , sa.admittime
    , sa.dischtime
    , sa.race
    
    -- Addmisions Features
    , add.length_of_stay
    , add.next_admittime
    , add.readmitted
    , add.is_white
    , add.race_group

    , prior.prior_hospital_visits_1yr
    , prior.prior_er_visits_1yr
  
    -- Patient Features
    , pat.anchor_age + DATETIME_DIFF(sa.admittime, DATETIME(pat.anchor_year, 1, 1, 0, 0, 0), YEAR) AS age
    , CASE 
          WHEN (pat.anchor_age + DATETIME_DIFF(sa.admittime, DATETIME(pat.anchor_year, 1, 1, 0, 0, 0), YEAR)) < 18 THEN -1  -- Pediatric Group
          WHEN (pat.anchor_age + DATETIME_DIFF(sa.admittime, DATETIME(pat.anchor_year, 1, 1, 0, 0, 0), YEAR)) < 30 THEN 0
          WHEN (pat.anchor_age + DATETIME_DIFF(sa.admittime, DATETIME(pat.anchor_year, 1, 1, 0, 0, 0), YEAR)) < 40 THEN 1
          WHEN (pat.anchor_age + DATETIME_DIFF(sa.admittime, DATETIME(pat.anchor_year, 1, 1, 0, 0, 0), YEAR)) < 50 THEN 2
          WHEN (pat.anchor_age + DATETIME_DIFF(sa.admittime, DATETIME(pat.anchor_year, 1, 1, 0, 0, 0), YEAR)) < 60 THEN 3
          WHEN (pat.anchor_age + DATETIME_DIFF(sa.admittime, DATETIME(pat.anchor_year, 1, 1, 0, 0, 0), YEAR)) < 70 THEN 4
          WHEN (pat.anchor_age + DATETIME_DIFF(sa.admittime, DATETIME(pat.anchor_year, 1, 1, 0, 0, 0), YEAR)) < 80 THEN 5
          ELSE 6
      END AS age_category
    , pat.gender
    , pat.dod
    
    -- ICU Features
    , icuf.first_icu_admit
    , icuf.last_icu_discharge
    , icuf.total_icu_stays
    , icuf.total_icu_hours
    , icuf.ICU_flag
    , icu.los_icu

    -- Procedures Features
    , proc.num_procedures
    , proc.major_surgery_flag
    , proc.cardiac_surgery_flag
    , proc.orthopedic_surgery_flag
    , proc.respiratory_support_flag
    , proc.neurosurgery_flag
    , proc.sepsis_infection_flag
    , proc.dialysis_flag
    , proc.blood_transfusion_flag
    , proc.ecmo_flag
    , proc.emergency_surgery_flag
    , proc.prolonged_ventilation_flag
    , proc.tracheostomy_flag
    , proc.shock_management_flag
    , proc.cpr_flag
    , proc.stroke_prevention_flag
    , proc.bowel_resection_flag
    , proc.colostomy_ileostomy_flag
    , proc.gastrostomy_tube_flag
    , proc.central_line_flag
    , proc.wound_debridement_flag

    -- Medications Features (Predictive)
    , med.num_medications
    , med.pressor_med_flag    
    , med.broad_spectrum_antibiotic_med_flag
    , med.antifungal_med_flag
    , med.inotrope_med_flag
    , med.antiarrhythmic_med_flag
    , med.nephrotoxic_med_flag
    , med.diuretic_med_flag
    , med.corticosteroid_med_flag
    , med.sedative_med_flag

    -- ED Features
    , ed.ed_acuity
    , ed.ed_chief_complaint   
    , ed.ed_admit_time  
    , ed.ed_discharge_time  
    
    -- Comorbidities & Complications
    , diag.complication_index
    , diag.complication_binary
    , diag.myocardial_infarct
    , diag.congestive_heart_failure
    , diag.peripheral_vascular_disease
    , diag.cerebrovascular_disease
    , diag.dementia
    , diag.chronic_pulmonary_disease
    , diag.rheumatic_disease
    , diag.peptic_ulcer_disease
    , diag.mild_liver_disease
    , diag.diabetes_without_cc
    , diag.diabetes_with_cc
    , diag.paraplegia
    , diag.renal_disease
    , diag.malignant_cancer
    , diag.severe_liver_disease
    , diag.metastatic_solid_tumor
    , diag.aids
    , diag.cms_flag
    
    -- Calculate the Charlson Comorbidity Score using the original
    -- weights from Charlson, 1987.
    , + diag.myocardial_infarct + diag.congestive_heart_failure
    + diag.peripheral_vascular_disease + diag.cerebrovascular_disease
    + diag.dementia + diag.chronic_pulmonary_disease
    + diag.rheumatic_disease + diag.peptic_ulcer_disease
    + GREATEST(diag.mild_liver_disease, 3 * diag.severe_liver_disease)
    + GREATEST(2 * diag.diabetes_with_cc, diag.diabetes_without_cc)
    + GREATEST(2 * diag.malignant_cancer, 6 * diag.metastatic_solid_tumor)
    + 2 * diag.paraplegia + 2 * diag.renal_disease
    + 6 * diag.aids AS charlson_comorbidity_index
    
    -- 30-Day Mortality Indicator
    , CASE 
        WHEN pat.dod IS NOT NULL 
             AND pat.dod BETWEEN sa.admittime AND DATE_ADD(sa.admittime, INTERVAL 30 DAY) 
        THEN 1 ELSE 0 
      END AS mortality_30_day
    
    , sapsii.max_sapsii
    , sirs.max_sirs
    --, sofa.max_sofa_24hours
    , oasis.max_oasis
    , lods.max_lods
    , apsiii.max_apsiii
    
    
FROM sampled_admissions AS sa
LEFT JOIN admissions_features AS add ON sa.subject_id = add.subject_id AND sa.hadm_id = add.hadm_id
LEFT JOIN prior_visits AS prior ON sa.subject_id = prior.subject_id AND sa.hadm_id = prior.hadm_id
LEFT JOIN diagnoses_features AS diag ON sa.hadm_id = diag.hadm_id
LEFT JOIN patient_features AS pat ON sa.subject_id = pat.subject_id

LEFT JOIN procedures_features AS proc ON sa.hadm_id = proc.hadm_id
LEFT JOIN medications_features AS med ON sa.hadm_id = med.hadm_id
LEFT JOIN ed_features AS ed ON sa.hadm_id = ed.hadm_id

LEFT JOIN icu_features AS icuf ON sa.hadm_id = icuf.hadm_id
LEFT JOIN icu_filtered icu ON sa.hadm_id = icu.hadm_id
LEFT JOIN sapsii_max sapsii ON sa.hadm_id = sapsii.hadm_id
LEFT JOIN sirs_max sirs ON sa.hadm_id = sirs.hadm_id
-- LEFT JOIN sofa_max sofa ON sa.hadm_id = sofa.hadm_id
LEFT JOIN oasis_max oasis ON sa.hadm_id = oasis.hadm_id
LEFT JOIN lods_max lods ON sa.hadm_id = lods.hadm_id
LEFT JOIN apsiii_max apsiii ON sa.hadm_id = apsiii.hadm_id;
"
)

if (!exists("base_features")) {
  source(here("scripts", "feature_sets.R"))
}

# Run the query
data_hosp <- dbGetQuery(con_hosp, sql_query_admin_samp)

# Load IPUMS Data 
library(here)
ipums_data <- read.csv(here("data", "processed", "IPUMS_data.csv"), stringsAsFactors = FALSE)

# Count rows before merge
message("Rows before merge: ", nrow(data_hosp))

# Perform left join on matching keys
data_hosp <- merge(data_hosp, ipums_data, 
                   by = c("race_group", "insurance_category", "marital_status_category"), 
                   all.x = TRUE)

# Verify count are the  same
message("Rows after merge: ", nrow(data_hosp))

### Data Cleaning 

## Ensure column names are valid in R
names(data_hosp) <- make.names(names(data_hosp))

# Convert weighted features to numeric and replace NA with 0
data_hosp[weighted_features] <- lapply(data_hosp[weighted_features], function(x) {
  x <- as.numeric(x)  # Ensure numeric format
  x[is.na(x)] <- 0    # Replace missing values with 0
  return(x)
})

data_hosp[binary_features] <- lapply(data_hosp[binary_features], function(x) {
  x[is.na(x)] <- 0
  return(as.numeric(as.character(x)))  # Convert to numeric (0/1) for ML models
})

# # Convert Target Variables to Factors with Proper Labels
data_hosp[target_vars] <- lapply(data_hosp[target_vars], function(x) {
  factor(x, levels = c(0, 1), labels = c("No", "Yes"))  # Convert to "No"/"Yes"
})

data_hosp[continuous_features] <- lapply(data_hosp[continuous_features], function(x) {
  x <- as.numeric(x)  # Explicitly convert to numeric
  x[is.na(x)] <- 0    # Replace missing continuous values with 0
  return(x)
})

# # Categorical Variables - Replace NA with "Unknown" and Convert to Factor
data_hosp[categorical_vars] <- lapply(data_hosp[categorical_vars], function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "Unknown"
  return(as.factor(x))  # Convert to factor
})

# Convert Categorical Variables to Numeric Encoding for Modeling
data_hosp$marital_status_numeric <- as.numeric(as.factor(data_hosp$marital_status_category))
data_hosp$insurance_numeric <- as.numeric(as.factor(data_hosp$insurance_category))
data_hosp$admission_location_numeric <- as.numeric(as.factor(data_hosp$admission_location_category))
data_hosp$discharge_location_numeric <- as.numeric(as.factor(data_hosp$discharge_location_category))

# ICU Features - Replace NA with Placeholder Dates
data_hosp[icu_date_features] <- lapply(data_hosp[icu_date_features], function(x) {
  x[is.na(x)] <- as.Date("1900-01-01")
  return(x)
})

# Emergency Department Features - Handle Missing Values
data_hosp$ed_chief_complaint[is.na(data_hosp$ed_chief_complaint)] <- "Unknown"
data_hosp$ed_admit_time[is.na(data_hosp$ed_admit_time)] <- as.Date("1900-01-01")
data_hosp$ed_discharge_time[is.na(data_hosp$ed_discharge_time)] <- as.Date("1900-01-01")

# Exclusion criteria: Remove patients with age_category == 0
data_hosp <- data_hosp[order(data_hosp$admittime), ]

# Remove all patients with age_category == 0 (ALl Targets)
data_hosp <- data_hosp[data_hosp$age_category != 0, ]
data_hosp <- data_hosp[order(data_hosp$admittime), ]

# # Remove fully correlated features for mortality target
# data_hosp_mort <- data_hosp[!(data_hosp$hospital_expire_flag == 1), ]
# data_hosp_mort <- data_hosp[!(data_hosp$discharge_location %in% c("HOSPICE", "DIED")), ]

data_ICU <- data_hosp[data_hosp$ICU_flag == 1 & data_hosp$total_icu_stays > 0, ]
data_cms <- data_hosp[data_hosp$cms_flag == 1, ]

suffix <- paste0("_", sample_percent)
assign(paste0("data_hosp", suffix), data_hosp, envir = .GlobalEnv)
assign(paste0("data_ICU", suffix), data_ICU, envir = .GlobalEnv)
assign(paste0("data_cms", suffix), data_cms, envir = .GlobalEnv)

# Save to RDS files with dynamic names
saveRDS(data_hosp, here("data", "processed", paste0("data_hosp", suffix, ".rds")))
saveRDS(data_ICU, here("data", "processed", paste0("data_ICU", suffix, ".rds")))
saveRDS(data_cms, here("data", "processed", paste0("data_cms", suffix, ".rds")))

message("Processed data saved as: data_hosp", suffix, ".rds, data_ICU", suffix, ".rds, data_cms", suffix, ".rds")
