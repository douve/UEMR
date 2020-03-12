

OS.main.path = function(OS=get_os(),user='user',oneDrive = F){
  if(OS=='osx' & !oneDrive) path = paste0('/Users/',user,'/Documents/') # Mac Dan
  if(OS=='osx' & oneDrive) path = paste0('/Users/',user,'/OneDrive - Instituto de Investigación del Sida IrsiCaixa/') # Mac Dan
  if(OS=='windows') path = 'Z:/' # PC Oriol
  if(OS=='linux') path = paste0('/run/',user,'/1000/gvfs/smb-share:server=epofs.fjgol.loc,share=douchi/') # Unix IDIAP
  return(path)
}
