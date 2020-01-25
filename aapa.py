# -*- coding: utf-8 -*-
"""
Created on Sat Nov 30 16:03:39 2019

@author: jajwalya
"""
from __future__ import print_function, division
import csv
import pandas as pd
# Import pairwise2 module
from Bio import pairwise2

# Import format_alignment method
from Bio.pairwise2 import format_alignment

data = open("C:/Users/jajwa/Downloads/seqdump (3).txt")
protein = data.read()
# Define two sequences to be aligned

X = protein[:100]
Y = "GKGTTYEGGVREPALAFWPGHIAPGVTHELASSLDLLPTLAALAGAPLP-NVTLDGFDLSPLLLGTGKSPRQSLFFYPSYPDEVRGVFAVRTGKYKAHFFTQGSAHSDTTA-------------DPACHASSSLTAHEPPLLYDLSKDPGENYNL"

# Get a list of the global alignments between the two sequences satisfying 
#the given scoring
# A match score is the score of identical chars, else mismatch score.
# Same open and extend gap penalties for both sequences.
alignments = pairwise2.align.globalms(X, Y, 2, -1, -0.5, -0.1)

# Use format_alignment method to format the alignments in the list
for a in alignments:
    print(format_alignment(*a))
    
######################################################################################
    import numpy as np
from string import *

x = protein[:100]
y = "GKGTTYEGGVREPALAFWPGHIAPGVTHELASSLDLLPTLAALAGAPLP-NVTLDGFDLSPLLLGTGKSPRQSLFFYPSYPDEVRGVFAVRTGKYKAHFFTQGSAHSDTTA-------------DPACHASSSLTAHEPPLLYDLSKDPGENYNL"

#-------------------------------------------------------
#This function returns to values for cae of match or mismatch
def Diagonal(n1,n2,pt):
    if(n1 == n2):
        return pt['MATCH']
    else:
        return pt['MISMATCH']

#------------------------------------------------------------   
#This function gets the optional elements of the aligment matrix and returns the elements for the pointers matrix.
def Pointers(di,ho,ve):

    pointer = max(di,ho,ve) #based on python default maximum(return the first element).

    if(di == pointer):
        return 'D'
    elif(ho == pointer):
        return 'H'
    else:
         return 'V'    

#--------------------------------------------------------
def NW(s1,s2,match = 1,mismatch = -1, gap = -2):
    penalty = {'MATCH': match, 'MISMATCH': mismatch, 'GAP': gap} #A dictionary for all the penalty valuse.
    n = len(s1) + 1 #The dimension of the matrix columns.
    m = len(s2) + 1 #The dimension of the matrix rows.
    al_mat = np.zeros((m,n),dtype = int) #Initializes the alighment matrix with zeros.
    p_mat = np.zeros((m,n),dtype = str) #Initializes the alighment matrix with zeros.
    #Scans all the first rows element in the matrix and fill it with "gap penalty"
    for i in range(m):
        al_mat[i][0] = penalty['GAP'] * i
        p_mat[i][0] = 'V'
    #Scans all the first columns element in the matrix and fill it with "gap penalty"
    for j in range (n):
        al_mat[0][j] = penalty['GAP'] * j
        p_mat [0][j] = 'H'
    #Fill the matrix with the correct values.

    p_mat [0][0] = 0 #Return the first element of the pointer matrix back to 0.
    for i in range(1,m):
        for j in range(1,n):
            di = al_mat[i-1][j-1] + Diagonal(s1[j-1],s2[i-1],penalty) #The value for match/mismatch -  diagonal.
            ho = al_mat[i][j-1] + penalty['GAP'] #The value for gap - horizontal.(from the left cell)
            ve = al_mat[i-1][j] + penalty['GAP'] #The value for gap - vertical.(from the upper cell)
            al_mat[i][j] = max(di,ho,ve) #Fill the matrix with the maximal value.(based on the python default maximum)
            p_mat[i][j] = Pointers(di,ho,ve)
    print(np.asmatrix(al_mat))
    
NW(x,y)


#######################################################################################


nw_score = []

pt ={'match': 1, 'mismatch': -1, 'gap': -1}

def mch(alpha, beta):
    if alpha == beta:
        return pt['match']
    elif alpha == '-' or beta == '-':
        return pt['gap']
    else:
        return pt['mismatch']

def needle(s1, s2):
    m, n = len(s1), len(s2)
    score = np.zeros((m+1, n+1))
    
    #Initialization
    for i in range(m+1):
        score[i][0] = pt['gap'] * i
    for j in range(n+1):
        score[0][j] = pt['gap'] * j
    
    #Fill
    for i in range(1, m + 1):
        for j in range(1, n + 1):
            diag = score[i-1][j-1] + mch(s1[i-1], s2[j-1])
            delete = score[i-1][j] + pt['gap']
            insert = score[i][j-1] + pt['gap']
            score[i][j] = max(diag, delete, insert)

    print('score matrix = \n%s\n' % score)
    align1, align2 = '', ''
    i,j = m,n
    
    #Traceback
    while i > 0 and j > 0:
        score_current = score[i][j]
        score_diag = score[i-1][j-1]
        score_left = score[i][j-1]
        score_up = score[i-1][j]
        
        nw_score.append(score_current)
        
        print('score_current: ',score_current)
        print('score_diag: ',score_diag)
        print('score_left: ',score_left)
        print('score_up: ',score_up)

        if score_current == score_diag + mch(s1[i-1], s2[j-1]):
            print('diag')
            a1,a2 = s1[i-1],s2[j-1]
            i,j = i-1,j-1
        elif score_current == score_up + pt['gap']:
            print('up')
            a1,a2 = s1[i-1],'-'
            i -= 1
        elif score_current == score_left + pt['gap']:
            print('left')
            a1,a2 = '-',s2[j-1]
            j -= 1
        print('%s ---> a1 = %s\t a2 = %s\n' % ('Add',a1,a2))
        align1 += a1
        align2 += a2
            

    while i > 0:
        a1,a2 = s1[i-1],'-'
        print('%s ---> a1 = %s\t a2 = %s\n' % ('Add',a1,a2))
        align1 += a1
        align2 += a2
        i -= 1
        
    while j > 0:
        a1,a2 = '-',s2[j-1]
        print('%s --> a1 = %s\t a2 = %s\n' % ('Add',a1,a2))
        align1 += a1
        align2 += a2
        j -= 1
    
    align1 = align1[::-1]
    align2 = align2[::-1]
    seqN = len(align1)
    sym = ''
    seq_score = 0
    ident = 0
    for i in range(seqN):
        a1 = align1[i]
        a2 = align2[i]
        if a1 == a2:
            sym += a1
            ident += 1
            seq_score += mch(a1, a2)
    
        else: 
            seq_score += mch(a1, a2)
            sym += ' '
        
    ident = ident/seqN * 100
    
    print('Identity = %2.1f percent' % ident)
    print('Score = %d\n'% seq_score)
    print(align1)
    print(sym)
    print(align2)

    nw_score.reverse()
    np.savetxt ('score.csv', score, delimiter = ',', fmt = '%s')
    np.savetxt("protein.csv", [ p for p in zip(nw_score, align1, align2)], delimiter=',',fmt='%s')
    
if __name__ == '__main__':
    needle(protein, "GKGTTYEGGVREPALAFWPGHIAPGVTHELASSLDLLPTLAALAGAPLP-NVTLDGFDLSPLLLGTGKSPRQSLFFYPSYPDEVRGVFAVRTGKYKAHFFTQGSAHSDTTA-------------DPACHASSSLTAHEPPLLYDLSKDPGENYNL")

    

#######################################################################################
import mysql.connector
import pandas as pd
from pandas import dataframe  

connection = mysql.connector.connect(host='localhost', database='protein', user='root', password='*******')
mycursor = connection.cursor()

mycursor.execute("CREATE TABLE aa_pairwise_scores (score NVARCHAR(20), left_i VARCHAR(11), right_i VARCHAR(11))")
mycursor.execute("DROP TABLE IF EXISTS aa_pairwise_scores")
mycursor.execute("CREATE TABLE aa_pairwise_scores (id1 INT AUTO_INCREMENT PRIMARY KEY, score INT(20), left_i VARCHAR(11), right_i VARCHAR(11))")

csv_data = pd.read_csv('protein.csv')
for row in csv_data.iterrows():
    pd.dataframe.astype({'score' : 'int32', 'left_i' : 'varchar11', 'right_i' : 'varchar11'})
    mycursor.execute('INSERT INTO aa_pairwise_scores VALUES (1, "%s",  "%s", "%s")', row)
#close the connection to the database.

    # the connection is not autocommitted by default, so we must commit to save our changes
    connection.commit()
    
    
mycursor.execute("CREATE TABLE blast_results (id1 INT AUTO_INCREMENT PRIMARY KEY, id2 INT AUTO INCREMENT PRIMARY KEY, protein1 NVARCHAR(20), protein2 NVARCHAR(20), score INT(20), SequenceLength INT(11), Gaps INT(11), Identical INT(11), Similar INT(11), protein_1_AAString_Aligned VARCHAR(1000), interprotein_Alignment VARCHAR(1000), proteins_designator VARCHAR(250))")

for row in protein:
    print(row)
    print("End")
    sql = "INSERT INTO blast_results ( %s ) VALUES (" + "%s,"*(len(row)-1) + "%s)"
    mycursor.execute(sql, tuple(row))

    # the connection is not autocommitted by default, so we must commit to save our changes
    connection.commit()
    
    
mycursor.execute("CREATE TABLE amino_acids (symbol VARCHAR AUTO_INCREMENT PRIMARY KEY(1), trigraph VARCHAR(3), name VARCHAR(45) )")

csv_data = pd.read_csv('amino_acids.csv')
for row in csv_data.iterrows():
    sql = "INSERT INTO amino_acids (`" +cols + "`) VALUES (" + "%s,"*(len(row)-1) + "%s)"
    cursor.execute(sql, tuple(row))

    # the connection is not autocommitted by default, so we must commit to save our changes
    connection.commit()
    
mycursor.execute("CREATE TABLE proteins (id INT(11), designator VARCHAR AUTO_INCREMENT PRIMARY KEY(25), name VARCHAR(45), AAString VARCHAR(1000) )")

for i,row in data.iterrows():
    sql = "INSERT INTO proteins" +cols + "`) VALUES (" + "%s,"*(len(row)-1) + "%s)"
    cursor.execute(sql, tuple(row))

    # the connection is not autocommitted by default, so we must commit to save our changes
    connection.commit()