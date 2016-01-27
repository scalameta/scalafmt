function(doc) {
    emit([doc.doc.gitInfo.commit, doc.doc.createdAt], doc.doc);
}
