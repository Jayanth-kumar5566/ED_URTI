import pandas
import matplotlib.pyplot as plt

'''
df=pandas.read_csv("importance_virus.csv")
df.sort_values(by='Virus', ascending=True,inplace=True)
plt.barh(df["Unnamed: 0"],df["Virus"])
plt.title("Feature Importance to predict VirusYes")
plt.xlabel("Importance")
plt.text(55,5,"Accuracy: $0. 603 \pm 0.00179$")
plt.show()
'''

df=pandas.read_csv("importance_crp.csv")
df.sort_values(by='Overall', ascending=True,inplace=True)
plt.barh(df["Unnamed: 0"],df["Overall"])
plt.title("Feature Importance to predict POCT CRP")
plt.xlabel("Importance")
plt.text(55,5,"$R^{2}: 0. 1607 \pm 4.53E-05$")
plt.show()
