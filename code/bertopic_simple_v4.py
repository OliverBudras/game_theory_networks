import pandas as pd
from bertopic import BERTopic
import pyreadr
from sentence_transformers import SentenceTransformer 
from sklearn.feature_extraction.text import CountVectorizer 


# Example DataFrame
data = pyreadr.read_r("v4/abstract_embeddings.RDS")[None] 

model = SentenceTransformer('all-MiniLM-L6-v2') 


# Prepare an empty list to store results
final_results = []

# Loop through each unique field
for discipline in data['discipline'].unique():
    print(f"Processing discipline: {discipline}")
    
    # Select articles for this field
    subset = data[data['discipline'] == discipline]
    articles = subset['text'].tolist()
    
    
    if len(articles) < 10:  # must be at least min_topic_size
       print(f"Skipping discipline {discipline}, not enough documents ({len(articles)})")
       subset = subset.copy()
       subset['topic'] = -1
       subset['topic_keywords'] = [[] for _ in range(len(subset))]
       final_results.append(subset)
       continue
    
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
    subset['topic'] = topics
    subset['topic_keywords'] = [topic_keywords.get(t, []) for t in topics]
    
    # Append to results
    out_path = f"v4/topics/{discipline}.RDS"
    pyreadr.write_rds(out_path, subset)

# Combine all results

    