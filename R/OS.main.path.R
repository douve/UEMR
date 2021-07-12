

OS.main.path = function(OS=get_os(),user='danouchi',cloud = F){
  if(OS=='osx' & !cloud) path = paste0('/Users/',user,'/Documents/') # Mac Dan
  if(OS=='osx' & cloud) path = paste0('/Users/',user,'/GoogleDrive/My Drive/') #Drive Dan
  if(OS=='windows') path = 'Z:/' # PC Oriol
  if(OS=='linux') path = paste0('/run/',user,'/1000/gvfs/smb-share:server=epofs.fjgol.loc,share=douchi/') # Unix IDIAP
  return(path)
}
