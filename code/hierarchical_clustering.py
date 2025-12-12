from bertopic import BERTopic 
from sklearn.feature_extraction.text import CountVectorizer 
import numpy as np 
from tqdm import tqdm 
import pyreadr 
from sentence_transformers import SentenceTransformer 
import pandas as pd 
import ast 

model = SentenceTransformer('all-MiniLM-L6-v2') 

data = pyreadr.read_r("v3/abstract_embeddings.RDS")[None] 

fields_of_interest =["Computer Science", "Business, Management and Accounting", "Decision Sciences", 
                     "Social Sciences", "Engineering", "Economics, Econometrics and Finance"] 


data_filt = data[data["field_1"].isin(fields_of_interest)] 

field_topic_models = {} 
field_topic_results = [] 


for field_name, group in data_filt.groupby("field_1"): 
    print(f"Processing field: {field_name} ({len(group)} docs)") 
    
    group_embeddings = np.vstack(group["embedding"].apply(ast.literal_eval).values) 
    
    topic_model = BERTopic(embedding_model=model, min_topic_size=3, verbose=False, nr_topics=50) 
    
    topics, probs = topic_model.fit_transform(group["text"].tolist(), group_embeddings)
    
    field_topic_models[field_name] = topic_model  
    
    sub_df = pd.DataFrame({"oa_id": group["oa_id"].values,"field": field_name, "doc_id": group.index, "topic": topics}) 
    
    field_topic_results.append(sub_df) 
    
    

field_topic_df = pd.concat(field_topic_results) 
field_topic_df = field_topic_df[field_topic_df["field"].isin(fields_of_interest)] 

hierarchies = {} 

for field, model in field_topic_models.items(): 
    docs_series = data_filt.loc[data_filt["field_1"] == field, "text"] 
    docs = docs_series.dropna().astype(str).str.strip().tolist() 
    docs = [doc for doc in docs if doc != ""]
    
    if docs: hierarchies[field] = model.hierarchical_topics(docs) 
    
    else: print(f"No valid documents for field '{field}', skipping.") 
    
    ### Visualization 
    
    model_1 = field_topic_models['Computer Science'] 
    fig = model_1.visualize_hierarchy() 
    fig.show(renderer="browser") 
    
    model_2 = field_topic_models['Business, Management and Accounting'] 
    fig = model_2.visualize_hierarchy() 
    fig.show(renderer="browser") 
    
    model_3 = field_topic_models['Decision Sciences'] 
    fig = model_3.visualize_hierarchy() 
    fig.show(renderer="browser") 
    
    
    model_4 = field_topic_models['Social Sciences'] 
    fig = model_4.visualize_hierarchy() 
    fig.show(renderer="browser") 
    
    model_5 = field_topic_models['Engineering'] 
    fig = model_5.visualize_hierarchy() 
    fig.show(renderer="browser") 
    
    model_6 = field_topic_models['Economics, Econometrics and Finance'] 
    fig = model_6.visualize_hierarchy() 
    fig.show(renderer="browser") 
    
    
topic_keywords = {} 

for field, model in field_topic_models.items(): 
    topic_keywords[field] = model.get_topic_info() 

subtopic_keywords = {}

for field, model in field_topic_models.items():
    info = model.get_topic_info()
    subtopic_keywords[field] = dict(zip(info["Topic"], info["Name"]))
    
hierarchies['Computer Science'].Parent_Name

parent_topics = {}

for field, df in hierarchies.items():
    parent_topics[field] = dict(zip(df["Topics"], df["Parent_ID"]))

    
parent_keywords = {}

for field, df in hierarchies.items():
    model = field_topic_models[field]
    # Map parent id → keyword name
    pk = { row["Topic"]: row["Name"] for _, row in model.get_topic_info().iterrows() }
    parent_keywords[field] = pk

taxonomy = {} 

for field, model in field_topic_models.items(): 
    info = model.get_topic_info() 
    for _, row in info.iterrows(): 
       taxonomy[field].append({ "topic_id": row["Topic"], "keywords": row["Name"], "size": row["Count"] })
    
    
    
    
final_df = field_topic_df.copy()

# Add subtopic keywords
final_df["subtopic_keywords"] = final_df.apply(
    lambda row: subtopic_keywords[row["field"]].get(row["topic"], None),
    axis=1
)

# Add parent topic ID
final_df["parent_topic"] = final_df.apply(
    lambda row: parent_topics[row["field"]].get(row["topic"], None),
    axis=1
)

# Add parent topic keywords
final_df["parent_keywords"] = final_df.apply(
    lambda row: parent_keywords[row["field"]].get(row["parent_topic"], None),
    axis=1
)
    
    
    
    
    