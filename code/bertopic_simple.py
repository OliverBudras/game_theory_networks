import pandas as pd
from bertopic import BERTopic
import pyreadr
from sentence_transformers import SentenceTransformer 
from sklearn.feature_extraction.text import CountVectorizer 


# Example DataFrame
data = pyreadr.read_r("v3/abstract_embeddings.RDS")[None] 

fields_of_interest =["Computer Science", "Business, Management and Accounting", "Decision Sciences", 
                     "Social Sciences", "Engineering", "Economics, Econometrics and Finance"] 


data_filt = data[data["field_1"].isin(fields_of_interest)] 

model = SentenceTransformer('all-MiniLM-L6-v2') 


# Prepare an empty list to store results
final_results = []

# Loop through each unique field
for field in data_filt['field_1'].unique():
    print(f"Processing field: {field}")
    
    # Select articles for this field
    subset = data_filt[data_filt['field_1'] == field]
    articles = subset['text'].tolist()
    
    # Initialize BERTopic
    vectorizer = CountVectorizer(stop_words="english", max_features=10000)
   
    topic_model = BERTopic(
    embedding_model=model,
    vectorizer_model=vectorizer,
    min_topic_size=3,
    verbose=False,
    nr_topics=10
    )
    
    # Fit the model on the subset
    topics, probs = topic_model.fit_transform(articles)
    
    # Get topic keywords
    topic_info = topic_model.get_topic_info()
    topic_keywords = {}
    for topic_num in topic_info['Topic'].unique():
        if topic_num == -1:
            continue  # Skip outlier topic
        keywords = topic_model.get_topic(topic_num)
        # Extract just the words
        topic_keywords[topic_num] = [word for word, _ in keywords]
    
    # Add topic and keywords to the subset
    subset = subset.copy()
    subset['topic'] = topics
    subset['topic_keywords'] = [topic_keywords.get(t, []) for t in topics]
    
    # Append to results
    final_results.append(subset)

# Combine all results
final_df = pd.concat(final_results, ignore_index=True)


pyreadr.write_rds("v3/field_topic_data.RDS", df=final_df)
    