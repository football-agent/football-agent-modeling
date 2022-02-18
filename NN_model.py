import pandas as pd
import numpy as np
import os
import torch
import os
from tqdm import tqdm
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix, classification_report

from torch import nn, optim
import torch.nn.functional as F
from torch.utils.data import DataLoader, TensorDataset


class Network(nn.Module):
    def __init__(self):
        super().__init__()

        # Inputs to hidden layer linear transformation
        self.l1 = nn.Linear(40, 60)
        self.l2 = nn.Linear(60, 60)
        self.l3 = nn.Linear(60, 40)
        self.l4 = nn.Linear(40, 10)

        # Output layer, 10 units - one for each digit
        self.output = nn.Linear(10, 1)

        # Define sigmoid activation and softmax output
        self.softmax = nn.Softmax(dim=1)

    def forward(self, x):
        # Pass the input tensor through each of our operations
        x = self.l1(x)
        x = F.relu(x)
        #         x = self.l2(x)
        #         x = F.relu(x)
        x = self.l3(x)
        x = F.relu(x)
        x = self.l4(x)
        x = F.relu(x)

        return self.output(x)


def train_model(model, epochs, loss_function, lr, train_loader, test_loader):
    loss_value = []
    optimizer = optim.Adam(params=model.parameters(), lr=lr)

    for i in range(epochs):
        for (x, y) in train_loader:
            x = x.to(device)
            # initialize the gradient of model parameters
            optimizer.zero_grad()
            # calculate the loss
            y_pred = model(x)

            loss = loss_function(y_pred, y.to(device))

            loss.backward()

            optimizer.step()
        if (i % 1) == 0:
            print('epoch: {},'.format(i) + 'loss: {}'.format(loss.detach()))



if __name__ == "__main__":
    print(os.getcwd())
    df = pd.read_csv('data.csv', sep=',', encoding='utf8', engine="python", index_col=0)
    df = df.dropna()
    df.head(5)

    fields = df.columns
    fields = [_f for _f in fields if _f not in ["season", "player", "contract_signing", "league"]]

    X = df[fields]
    y = df[['wage_eur']]
    X.dtypes
    X = torch.tensor(np.array(X), dtype=torch.float)
    y = torch.tensor(np.array(y).reshape(-1, 1), dtype=torch.float)

    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)
    # X_train = torch.from_numpy(X_train.to_numpy()).float()
    # y_train = torch.squeeze(torch.from_numpy(y_train.to_numpy()).float())

    # X_test = torch.from_numpy(X_test.to_numpy()).float()
    # y_test = torch.squeeze(torch.from_numpy(y_test.to_numpy()).float())

    print(X_train.shape, y_train.shape)
    print(X_test.shape, y_test.shape)
    train_dataset = TensorDataset(X_train, y_train)
    test_dataset = TensorDataset(X_test, y_test)

    train_loader = DataLoader(train_dataset, batch_size=64)
    test_loader = DataLoader(test_dataset, batch_size=32)

    device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")

    net = Network().to(device)
    loss_function = nn.MSELoss()

    train_model(net, 100, loss_function, 1e-5, train_loader, test_loader)
