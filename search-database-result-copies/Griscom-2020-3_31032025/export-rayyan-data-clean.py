import pandas as pd
import re

# Load the CSV file
df = pd.read_csv('articles.csv', header=None, names=['raw'])

# Function to extract the inclusion decision, labels, and user notes
def extract_info(text):
    # Ensure text is a string (handle NaNs or floats)
    if not isinstance(text, str):
        text = str(text)

    inclusion = None
    labels = None
    notes = None

    # Extract inclusion decision (e.g., Included or Excluded)
    incl_match = re.search(r'RAYYAN-INCLUSION:.*?"\w+"\s*=>\s*"(\w+)"', text)
    if incl_match:
        inclusion = incl_match.group(1)

    # Extract labels (everything after RAYYAN-LABELS:)
    label_match = re.search(r'RAYYAN-LABELS:\s*([^\|]+)', text)
    if label_match:
        labels = label_match.group(1).strip()

    # Extract user notes if present (not in all rows)
    notes_match = re.search(r'USER-NOTES:\s*(.+)', text)
    if notes_match:
        notes = notes_match.group(1).strip()

    return pd.Series([inclusion, labels, notes])

# Apply the extraction function to the 'raw' column, converting non-string values to strings
df[['inclusion', 'labels', 'notes']] = df['raw'].apply(lambda x: extract_info(str(x)))

# Save the cleaned data to a new CSV file, keeping the original 'raw' column
df.to_csv('cleaned_rayyan_with_raw.csv', index=False)

# Print the cleaned DataFrame to check
print(df.head())
