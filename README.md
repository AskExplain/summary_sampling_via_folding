# Sample summary through folding reduces runtime, retains accuracy

Summarising or Folding the samples reduces the total number of samples to a manageable amount to run on modern day machines.

The idea supposes that when there are many samples, some samples cluster and thus share more relevant information - making them nearer neighbours. The concept of summarising concentrates the sample information, akin to folding a piece of paper to reduce the total area. Running an algorithm on the folded samples then gives an [output] in the reduced sample space. The next step is to unfold the [output] to match the structure of the full samples in the original data.

The example that summarises samples here is called fold sampling, or folding.
