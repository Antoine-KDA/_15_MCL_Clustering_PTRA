import xml.etree.ElementTree as ET
import csv
import pandas as pd

def xml_to_csv(xml_file_path, csv_file_path):
    """
    Convert XML file to CSV format
    """
    try:
        # Parse the XML file
        tree = ET.parse(xml_file_path)
        root = tree.getroot()
        
        # Lists to store extracted data
        data = []
        
        # Function to recursively extract data
        def extract_elements(element, parent_path=""):
            current_path = f"{parent_path}/{element.tag}" if parent_path else element.tag
            
            # Get element info
            row = {
                'element_path': current_path,
                'element_tag': element.tag,
                'element_text': element.text.strip() if element.text else "",
                'attributes': str(element.attrib) if element.attrib else ""
            }
            
            # Add individual attributes as columns
            for attr_name, attr_value in element.attrib.items():
                row[f'attr_{attr_name}'] = attr_value
                
            data.append(row)
            
            # Recursively process children
            for child in element:
                extract_elements(child, current_path)
        
        # Extract all elements
        extract_elements(root)
        
        # Convert to DataFrame and save as CSV
        df = pd.DataFrame(data)
        df.to_csv(csv_file_path, index=False)
        
        print(f"Successfully converted {xml_file_path} to {csv_file_path}")
        print(f"Total rows: {len(df)}")
        print(f"Columns: {list(df.columns)}")
        
        return df
        
    except Exception as e:
        print(f"Error converting XML to CSV: {str(e)}")
        return None

def extract_icd_codes(xml_file_path, csv_file_path):
    """
    Specifically extract ICD-10 codes from the XML structure
    """
    try:
        tree = ET.parse(xml_file_path)
        root = tree.getroot()
        
        codes_data = []
        
        # Find all diag elements (ICD codes)
        for diag in root.findall('.//diag'):
            code_info = {
                'code': diag.get('code', ''),
                'description': diag.text.strip() if diag.text else '',
                'type': 'diagnosis'
            }
            codes_data.append(code_info)
        
        # Find all section elements
        for section in root.findall('.//section'):
            section_info = {
                'code': section.get('id', ''),
                'description': section.find('.//desc').text.strip() if section.find('.//desc') is not None else '',
                'type': 'section'
            }
            codes_data.append(section_info)
            
        # Find all chapter elements
        for chapter in root.findall('.//chapter'):
            chapter_info = {
                'code': chapter.get('id', ''),
                'description': chapter.find('.//desc').text.strip() if chapter.find('.//desc') is not None else '',
                'type': 'chapter'
            }
            codes_data.append(chapter_info)
        
        # Convert to DataFrame and save
        df = pd.DataFrame(codes_data)
        df = df[df['code'] != '']  # Remove empty codes
        df.to_csv(csv_file_path, index=False)
        
        print(f"Successfully extracted ICD codes to {csv_file_path}")
        print(f"Total codes: {len(df)}")
        
        return df
        
    except Exception as e:
        print(f"Error extracting ICD codes: {str(e)}")
        return None

# File paths
xml_file = r"c:\Users\kwhr625\Box\Personal\Old_Studies\MCL\ptra\ptra_test\icd10cm_tabular_2022.xml"
csv_basic = r"c:\Users\kwhr625\Box\Personal\Old_Studies\MCL\MCL_Clustering_PTRA\icd10cm_basic.csv"
csv_codes = r"c:\Users\kwhr625\Box\Personal\Old_Studies\MCL\MCL_Clustering_PTRA\icd10cm_codes.csv"

# Run the conversions
print("Converting XML to CSV (basic extraction)...")
df_basic = xml_to_csv(xml_file, csv_basic)

print("\nExtracting ICD codes specifically...")
df_codes = extract_icd_codes(xml_file, csv_codes)

print("\nConversion completed!")