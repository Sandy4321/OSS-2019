#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
    The clustering.py is responsible for the clusterization
    of the newcomers inflow (time series), using the K-Spectral Algorithm.
'''

__author__ =  'Felipe Fronchetti'
__contact__ = 'fronchetti@usp.br'

from pyksc import ksc
from pyksc import metrics
import numpy
from csv import reader, DictReader, DictWriter
import matplotlib.pyplot as plt

class TSClusterization(object):
    def __init__(self, projects, time_series, dataset_folder, csv_folder, images_folder):
        self.dataset_folder = dataset_folder
        self.csv_folder = csv_folder
        self.images_folder = images_folder

        self.projects = projects
        self.time_series = numpy.array(time_series)
        self.clusters_by_time_series = {}
        self.centroids = None
        self.assign = None
        self.best_shift = None
        self.cent_dists = None

    def calculate_beta_cv(self):
        '''
            We use the βCV heuristic to define the best number k of clusters.
            βCV is defined as the ratio of the coefficient of variation of the intracluster
            distances and the coefficient of variation of the intercluster distances.
            The smallest value of k after which the βCV ratio remains roughly stable should be selected.
        '''

        if self.assign is not None:
            return metrics.beta_cv(self.time_series, self.assign)

    def get_clusters(self, number_of_clusters):
        self.centroids, self.assign, self.best_shift, self.cent_dists = ksc.ksc(self.time_series, number_of_clusters)

        if self.assign is not None:
            for series, cluster in zip(self.time_series , self.assign):
                if cluster in self.clusters_by_time_series.keys():
                    self.clusters_by_time_series[cluster].append(series)
                else:
                    self.clusters_by_time_series[cluster] = [series]

    def plot_beta_cv(self, min_clusters=2, max_clusters=16):
        k_clusters = [k_i for k_i in range(min_clusters, max_clusters)]
        beta_cv = []

        for k_i in k_clusters:
            self.get_clusters(k_i)
            beta_cv.append(self.calculate_beta_cv())

        figure = plt.figure()
        plt.plot(k_clusters, beta_cv, color='black')
        plt.xlabel('# Clusters')
        plt.ylabel(r'$\beta$cv')
        plt.title(r'$\beta$cv for 2 $\leq$ k $\leq$ 15')
        figure.savefig(self.images_folder + '/beta_cv.eps', bbox_inches='tight', format='eps', dpi=1000)

    def plot_clusters(self):
        number_of_weeks = 0
        print self.clusters_by_time_series.keys()

        for cluster in self.clusters_by_time_series.keys():
            figure = plt.figure()

            for project_time_series in self.clusters_by_time_series[cluster]:
                project_time_series = [0 if i == 0.1 else int(i) for i in project_time_series]

                if number_of_weeks == 0:
                    number_of_weeks = [i for i in range(len(project_time_series), 0, -1)]

                plt.plot(number_of_weeks, project_time_series, color='black')

            max_newcomers_entry = max([project_time_series[-1] for project_time_series in self.clusters_by_time_series[cluster]])

            if max_newcomers_entry < 500:
                plt.ylim([0, 50])
            else:
                plt.ylim([0, 3300])

            plt.ylabel('# Newcomers', size = 20)
            plt.xlabel('Week', size = 20)
            plt.tick_params(axis='both', which='major', labelsize=20)
            plt.tick_params(axis='both', which='minor', labelsize=20)
            figure.savefig(self.images_folder + '/cluster_' + str(cluster + 1) + '.eps', bbox_inches='tight', format='eps', dpi=1000)

    def plot_centroids(self):
        number_of_weeks = None

        for cluster, centroid in zip(range(0, len(self.centroids)), self.centroids):
            if number_of_weeks is None:
                number_of_weeks = [-i for i in range(len(centroid), 0, -1)]

            figure = plt.figure()
            plt.plot(number_of_weeks, centroid, color='black')
            plt.ylabel('# Newcomers', size = 20)
            plt.xlabel('Week', size = 20)
            plt.tick_params(axis='both', which='major', labelsize=20)
            plt.tick_params(axis='both', which='minor', labelsize=20)
            plt.ylim([0, 0.5])
            figure.savefig(self.images_folder + '/centroid_' + str(cluster + 1) + '.eps', bbox_inches='tight', format='eps', dpi=1000)

    def export_clusters_data(self):
        summary_file = open(self.csv_folder + '/summary.csv', 'r')
        summary = DictReader(summary_file)

        for project_row in summary:
             for project, time_series, cluster in zip(self.projects, self.time_series, self.assign):
                if project_row['name'] == str(project):
                    project_row['cluster'] = cluster + 1
        
        with open(self.csv_folder + '/summary.csv', 'w') as summary_file:
            writer = DictWriter(summary_file, fieldnames=summary.fieldnames + ['cluster'])
            writer.writeheader()
            
            for project_row in summary:
                writer.writerow(project_row)

if __name__ == '__main__':
    dataset_folder = '../../dataset'
    csv_folder = '../../spreadsheets'
    images_folder = '../../plots/clusters/'

    newcomers_inflow_file = open(csv_folder + '/newcomers_inflow.csv' , 'r')
    newcomers_inflow = DictReader(newcomers_inflow_file)    
    projects = []
    time_series = []

    for project in newcomers_inflow:
        project_name = project['project']
        project_time_series = [project[column] for column in project if column != 'project']
        project_time_series = [0.1 if value == '0' else int(value) for value in project_time_series]
        projects.append(project_name)
        time_series.append(project_time_series)

    k_spectral = TSClusterization(projects, time_series, dataset_folder, csv_folder, images_folder)
    k_spectral.get_clusters(4)
    k_spectral.plot_beta_cv()
    k_spectral.plot_clusters()
    k_spectral.plot_centroids()
    k_spectral.export_clusters_data()