import pyreadr
import pandas as pd
from sentence_transformers import SentenceTransformer

# Read Data
data = pyreadr.read_r("v4/node_field_data.RDS")[None]
data["text"] = data["title"] + "." +  data["abstract"]

# Create Embeddings

model = SentenceTransformer('all-MiniLM-L6-v2')
embeddings = model.encode(data["text"].tolist(), show_progress_bar=True)
data["embedding"] = embeddings.tolist()

pyreadr.write_rds("v4/abstract_embeddings.RDS", data)
