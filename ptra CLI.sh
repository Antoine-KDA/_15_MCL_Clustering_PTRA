
# Example of how to run ptra from command line: Charlotte's example
./ptra fake-patients.csv icd10cm_tabular_2022.xml fake-diagnoses.csv ./test1/ --nofAgeGroups 6 --lvl 2 --maxYears 40 --minYears 0.5 --minPatients 10 --maxTrajectoryLength 5 --minTrajectoryLength 3 --name exp1 --iter 100 —cluster

# Antoine implementation
./ptra ptra_test/ptra_patients_data_MAIN.csv ptra_test/icd10cm_tabular_2022.xml ptra_test/ptra_diagnosis_data_MAIN.csv ./MCL_Ant/ --nofAgeGroups 6 --lvl 2 --maxYears 40 --minYears 0.5 --minPatients 10 --maxTrajectoryLength 5 --minTrajectoryLength 3 --name exp1 --iter 100 --cluster