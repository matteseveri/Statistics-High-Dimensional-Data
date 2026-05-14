# Statistics High Dimensional Data

Authors: Matteo Severi - Gianluca Antonio Spennacchio - Maria Gnoli - Alessandro Ceschel 

Course: Statistics for High Dimensional Data — University of Bologna

The WDI dataset (World Bank, 1993) contains 8 sustainability indicators for 100 countries. This project applies PCA to reduce dimensionality, K-means and hierarchical clustering to identify country groups, and LDA/QDA to assess their robustness.

Three principal components are retained, capturing a sustainability trade off gradient, cumulative environmental stress, and energy/capital intensity. Clustering identifies four distinct country profiles: balanced moderate economies, pollution burdened net borrowing economies, fossil fuel driven economies with strong reinvestment, and extraction intensive resource exporting economies. Discriminant analysis confirms the cluster structure, with LDA achieving 96% accuracy on original variables and 91% on principal components.
