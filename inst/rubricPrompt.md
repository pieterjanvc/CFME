# TASK
You will be assessing the quality of a provided a clinical clerkship evaluation written by a supervisor
Detect any of the listed competencies in the provided evaluation using the rubric below

# RUBRIC

## Competencies

### 1. Medical Knowledge
Demonstrate understanding of foundational principles that underlie the medical sciences and apply this knowledge in care of individuals and populations through clinical-problem solving.

### 2. Patient Care
Provide compassionate, inclusive, trauma-informed, and affirming patient-centered care in order to promote and improve health through accurate medical history taking and physical examination, efficiently  evaluating patient data as well as medical literature to establish a diagnosis and management plan.

### 3. Interpersonal and Communication Skills
Form collaborative and trusting relationships with patients, caregivers and all members of the healthcare team through effective oral and written professional communication.

### 4. Critical and Informed Thinking, Adaptive Expertise and Scientific Inquiry
Evaluate, analyze and apply new and existing knowledge with advances and discoveries in biomedical, clinical, social, behavioral and population and data sciences through continuous self-directed learning and scholarly work that aims to advance both the medical field at large and specific patient care.

### 5. Professionalism
Exemplify the professional values of medicine, including compassion, integrity, social responsibility and respect for all persons and identities towards both colleagues and patients.

### 6. Health Equity
Demonstrate the skills needed to address and improve health inequities through advocacy in partnership with patients and the medical community to improve health outcomes at the individual, organizational and societal levels with understanding of and sensitivity to the social determinants of health.

### 7. Skill
Technical skills

## compScores: Individual competency scores

For each detected competency collect the following data points:

### cID: Competency ID (1-n)

### spec: Specificity score:
- 1: briefly mentioned (keywords or bullet points) with no supporting evidence
- 2: more detailed description but no supporting evidence
- 3: more detailed description with one piece of supporting evidence
- 4: comprehensive description with multiple pieces of supporting evidence

### text: Pieces of verbatim text from the assessment supporting the specificity score. Only use minimal number of words needed.

## overallScores: Overall scores

These scores are for the evaluation as a whole

### util: Utility score indicating how useful the feedback is to the student:
- 1: low/not useful: Uses 3rd person, minimal specific information, often vague
- 2: moderately useful: Specific to the student but hard to act upon
- 3: highly useful: Very specific and directly applicable to professional improvement

### sent: Evaluator sentiment score for the overall review
- 1: clearly negative or red flags
- 2: slightly negative or coded language indicating potential critisism
- 3: neutral or not enough information to indicate sentiment
- 4: generic positive language (e.g. aspecific superlatives)
- 5: specific positive language indicating the reviewer's effort to make the student stand out

# TO RETURN
A valid JSON string without any other code markup using the template below.
Replace any example values with the real ones!
{"compScores": [{"cID": 1,"spec": 1,"text": ["some text evidence","other text evidence"]},{}],"util": 1,"sent": 1}
