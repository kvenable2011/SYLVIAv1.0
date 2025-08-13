import traceback
#import geopandas as gpd
import requests
import json,pickle
import os
import multiprocessing as mp
import numpy as np
import html
import logging

class GeogTool:
    def __init__(self,):
        '''try:
            self.comiddatadir
            
        except:
            self.comiddatadir=os.path.join(os.getcwd(),'data','comid_data')
            if not os.path.exists(self.comiddatadir):os.makedirs(self.comiddatadir)'''
        
        try:
            self.logger = logging.getLogger(__name__)
            self.logger.info('GeogTool starting')
        except:
            self.cwd=os.getcwd()
            self.logdir=os.path.join(self.cwd,'log'); 
            if not os.path.exists(self.logdir):os.mkdir(self.logdir)
            handlername=os.path.join(self.logdir,'GeogTool.log')
            logging.basicConfig(
                handlers=[logging.handlers.RotatingFileHandler(handlername, maxBytes=10**7, backupCount=1)],
                level=logging.DEBUG,
                format="[%(asctime)s] %(levelname)s [%(name)s.%(funcName)s:%(lineno)d] %(message)s",
                datefmt='%Y-%m-%dT%H:%M:%S')
            self.logger = logging.getLogger(handlername)
            self.logger.info('GeogTool starting new logger',exc_info=True)
        try: self.geogdatadir
        except:
            self.geogdatadir=os.path.join(os.getcwd(),'data')
        self.NHDplus_path=os.path.join(self.geogdatadir,'NHDplus.pickle')
        self.huc12comiddict_path=os.path.join(self.geogdatadir,'huc12comiddict.pickle')
        self.NHDdbf_path=os.path.join(self.geogdatadir,'HUC12_PU_COMIDs_CONUS.dbf')
        self.NHDhuchuc_path=os.path.join(self.geogdatadir,'NHDhuchuc.pickle')
        
    def getpickle(self,path):
        with open(path,'rb') as f:
            thefile=pickle.load(f)
        return thefile
    
    def savepickle(self,obj,path):
        with open(path,'wb') as f:
            pickle.dump(obj,f)
        
        
        
        
        
    def gethuc12comiddict(self):
        try: 
            huc12comiddict_path=self.huc12comiddict_path
            self.huc12comiddict=self.getpickle(huc12comiddict_path)
            self.logger.info(f'opening {self.huc12comiddict_path} with length:{len(self.huc12comiddict)} and type:{type(self.huc12comiddict)}')
        except: 
            self.logger.exception(f"{self.huc12comiddict_path} exists but could not open, rebuilding")
            self.buildNHDplus()
        
        return self.huc12comiddict
    
    
    
                
    def select_random_huc8(self,count=2,huc2list=None,seed=0):
        if not seed is None:
            np.random.seed(seed)
        try:
            huchuc=self.getpickle(self.NHDhuchuc_path)
        except:
            huchuc=self.build_huchuc()
        huc2_huc8dict=huchuc['huc2_huc8dict']
        huc8_huc12dict=huchuc['huc8_huc12dict']
        huc2_huc8dict_select={}
        huc8_huc12dict_select={}
        if not huc2list is None:
            for huc2,huc8list in huc2_huc8dict.items():
                if huc2 in huc2list:
                    huc2_huc8dict_select[huc2]=huc8list
                    for huc8 in huc8list:
                        huc8_huc12dict_select[huc8]=huc8_huc12dict[huc8]
            huc2_huc8dict=huc2_huc8dict_select
            huc8_huc12dict=huc8_huc12dict_select
        huc8list=[huc8 for huc8 in huc8_huc12dict]
        huc8count=len(huc8list)
        # np.random.randint(0,huc8count-1,[count])
        selectarray=np.arange(0,huc8count, dtype=int)
        np.random.shuffle(selectarray)
        huc8_huc12dict_select={}
        huc8_selection=[huc8list[selectarray[ii]] for ii in selectarray[:count]]
        huc8_huc12dict_finalselect={}
        for huc8 in huc8_selection:
            huc8_huc12dict_finalselect[huc8]=huc8_huc12dict[huc8]
        return huc8_huc12dict_finalselect
        
            
        
        
            
        
        
    def build_huchuc(self):
        
        try: self.huc12comiddict
        except: self.gethuc12comiddict()
        huc2_huc8dict={}
        huc8_huc12dict={}
        
        for huc12 in self.huc12comiddict:
            a_huc8=huc12[0:8]
            a_huc2=huc12[0:2]
            try: 
                huc8_huc12dict[a_huc8].append(huc12)
            except: 
                huc8_huc12dict[a_huc8]=[huc12]
                try:
                    huc2_huc8dict[a_huc2].append(a_huc8)
                except:
                    huc2_huc8dict[a_huc2]=[a_huc8]
        huchuc={'huc2_huc8dict':huc2_huc8dict,'huc8_huc12dict':huc8_huc12dict}
        self.savepickle(huchuc,self.NHDhuchuc_path)
        return huchuc
        
            
            
    
    def buildNHDplus(self,setNHDplus_attribute=0):
        
        savefilename=self.NHDplus_path
        if os.path.exists(savefilename):
            try: 
                NHDplus=self.getpickle(savefilename)
                self.logger.info(f'opening {savefilename} with length:{len(NHDplus)} and type:{type(NHDplus)}')
                # self.logger.info(NHDplus)
            except: 
                self.logger.info(f"{savefilename} exists but could not open, rebuilding")
                
        try: NHDplus
        except:
            filename=self.NHDdbf_path
            self.logger.info(f'starting read of {filename}')
            NHDplus=gpd.read_file(filename)
            self.logger.info('finished read of NHDplus')
            self.logger.info(f'opened {filename} with length:{len(NHDplus)} and type:{type(NHDplus)}')
        if os.path.exists(self.huc12comiddict_path):
            try: 
                self.huc12comiddict=self.getpickle(self.huc12comiddict_path)
                self.logger.info(f'opening {self.huc12comiddict_path} with length:{len(self.huc12comiddict)} and type:{type(self.huc12comiddict)}')
                # self.logger.info(self.huc12comiddict)
                if setNHDplus_attribute:
                    self.NHDplus=NHDplus
                return 
            except: 
                self.logger.info(f"{savefilename} exists but could not open, rebuilding")

        self.logger.info('NHDplus.columns.values',NHDplus.columns.values)
        
        NHDplusHUC12array=NHDplus.loc[:,('HUC12')].to_numpy(dtype='str')
        self.logger.info('buildng huc12comiddict')
        huc12dict={}
        for comid_idx,huc12 in enumerate(NHDplusHUC12array):
            if len(huc12)==11:huc12='0'+huc12
            comid=NHDplus.loc[comid_idx,'COMID']
            if huc12 in huc12dict:
                huc12dict[huc12].append(comid)
            else: 
                huc12dict[huc12]=[comid]
        self.savepickle(huc12dict,self.huc12comiddict_path)
        self.savepickle(NHDplus,savefilename)
        self.huc12comiddict=huc12dict
        if setNHDplus_attribute:
            self.NHDplus=NHDplus
        return 
                
    
    def getstreamcat(self,comidlist):
        #url = "https://ofmpub.epa.gov/waters10/streamcat.jsonv25?pcomid={}&pLandscapeMetricType=Topography"
        url = "https://ofmpub.epa.gov/waters10/streamcat.jsonv25?pcomid={}"
        #url="https://ofmpub.epa.gov/waters10/Watershed_Characterization.Control?pComID={}"
        if type(comidlist) is str:
            comidlist=[comidlist]
        comidcount=len(comidlist)
        huc12datadict={}
        for idx,comid in enumerate(comidlist):
            self.logger.info(f'starting {idx}/{comidcount}')
            result=requests.get(url.format(str(comid)))
            self.logger.info(f'retrieved {idx}/{comidcount}')
            success=0
            self.logger.info(f'type(result):{type(result)}')
            try:
                data=result.text
                try:
                    comiddatadict=json.loads(data)
                except:
                    self.logger.exception(f'failed to json.loads. comid:{comid}')
                    datadict={'fail':data}
            except:
                self.logger.exception(f'failed to result.text. comid:{comid}')
                self.logger.debug(f'{data}')
                comiddatadict=result
            huc12datadict[str(comid)]=comiddatadict
            self.logger.debug(f'comid:{comid},self.huc8:{self.huc8}datadict:{datadict}')
        infodict={'streamcatdata':huc12datadict}
        
        return infodict
    
    def getNHDplus(self,huc12):
        try: self.NHDplus
        except: self.buildNHDplus(setNHDplus_attribute=1)
        huc12dataframerows=self.NHDplus.loc[self.NHDplus['HUC12']==huc12]
        self.logger.info(f'type(huc12dataframerows):{type(huc12dataframerows)}')
        jsonfile=json.loads(huc12dataframerows.to_json())
        infodict={'NHDplusdata':jsonfile}
        return infodict
        
        
        
if __name__=="__main__":
    gt=GeogTool()
    gt.buildNHDplus()
    huc12list=[key for key in gt.huc12comiddict]
    self.logger.info('huc12list[0:10]',huc12list[0:10])
    self.logger.info('gt.huc12comiddict[huc12list[0]]',gt.huc12comiddict[huc12list[0]])
    self.logger.info('gt.huc12comiddict[huc12list[-1]]',gt.huc12comiddict[huc12list[-1]])
    self.logger.info('gt.huc12comiddict[huc12list[-1]]',gt.huc12comiddict['030701010307'])
    
    
                
    {
 "cells": [
  {
   "cell_type": "code",
   "execution_count": 1,
   "metadata": {},
   "outputs": [],
   "source": [
    "from geogtools import GeogTool"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 2,
   "metadata": {},
   "outputs": [],
   "source": [
    "test=GeogTool()"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 3,
   "metadata": {},
   "outputs": [],
   "source": [
    "comidlist=[13009636]"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 4,
   "metadata": {},
   "outputs": [],
   "source": [
    "infodict=GeogTool().getstreamcat(comidlist)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 11,
   "metadata": {},
   "outputs": [],
   "source": [
    "jsondata=json.loads(infodict['streamcatdata'][0])\n",
    "output=jsondata['output']"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 12,
   "metadata": {},
   "outputs": [],
   "source": [
    "import json\n",
    "with open('fullstreamcat_json.json','w') as f:\n",
    "    json.dump(output,f)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": []
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "Python 3",
   "language": "python",
   "name": "python3"
  },
  "language_info": {
   "codemirror_mode": {
    "name": "ipython",
    "version": 3
   },
   "file_extension": ".py",
   "mimetype": "text/x-python",
   "name": "python",
   "nbconvert_exporter": "python",
   "pygments_lexer": "ipython3",
   "version": "3.7.4"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 4
}
       
            
            
            
            